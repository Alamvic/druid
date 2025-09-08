Class {
	#name : 'DRZ3ConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'cfg',
		'memo',
		'assertions',
		'ctxt',
		'signedIntOverflowIsUB'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph [

	^ self new initializeFor: aDRControlFlowGraph
]

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean [

	^ self new initializeFor: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> adaptSize: op1 with: op2 andDo: aBlock [
	| _op1 _op2 |
	op1 isBitVector ifFalse: [ ^ aBlock value: op1 value: op2 ].
	
	_op1 := op1.
	_op2 := op2.
	op1 sort length < op2 sort length ifTrue: [ _op1 := op1 "signExtend:" zeroExtend: op2 sort length - op1 sort length ].
	op2 sort length < op1 sort length ifTrue: [ _op2 := op2 "signExtend:" zeroExtend: op1 sort length - op2 sort length ].
	
	^ aBlock value: _op1 value: _op2.
	
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> addZ3ConstraintFrom: aDRValue [

	aDRValue acceptVisitor: self
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> drValAsZ3Val: aDRValue ifUnsupported: aBlock [

	aDRValue type isIntegerType ifTrue: [
		^ aDRValue name toBitVector: aDRValue type numberOfBits ].

	aDRValue type isBooleanType ifTrue: [ ^ aDRValue name toBool ].

	aDRValue type isFloatType ifTrue: [ "^ aDRValue name toReal"
	"TODO I think MachineArithmetic doesn't have floats yet? only Reals"].
	
	(aDRValue type isUndefined or: [aDRValue type isFloatType]) ifTrue: [ aBlock value ].
	
	self error: 'Unsupported type'
]

{ #category : 'initialization' }
DRZ3ConstraintSolver >> initializeFor: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean [

	cfg := aDRControlFlowGraph.
	
	memo := IdentityDictionary new.
	
	signedIntOverflowIsUB := aBoolean.
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> isSatisfiable: aDRPiNode [

	| solver retval |

	assertions := OrderedCollection new.

	ctxt := Z3Context global.
	solver := ctxt mkSolver.

	self addZ3ConstraintFrom: aDRPiNode.

	assertions do: [ :a | solver assert: a ].
	retval := solver check.

	solver release.
	^ retval ifNil: [ true ]
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> recurseOnlyPis: aDRValue [

	| currentVal |
	currentVal := aDRValue.
	[ currentVal isPiNode and: [ currentVal operand isPhiFunction not ] ]
		whileTrue: [
			| res op |
			res := self drValAsZ3Val: currentVal ifUnsupported: [ ^ nil ].
			op := self drValAsZ3Val: currentVal operand ifUnsupported: [ ^ nil ].

			assertions add: (res === op and: (currentVal asZ3Constraint: self)).
			self recurseOnlyPis: currentVal constraint otherValue.
			currentVal := currentVal operand ].

	(currentVal isCopy or: [currentVal isConstant]) ifFalse: [ ^nil ].

	currentVal := currentVal simpleConstantFold asDRValue.
	currentVal isConstant ifTrue: [
		assertions add: (self drValAsZ3Val: aDRValue ifUnsupported: [ ^ nil ]) === currentVal value ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitAddition: aDRAdd [

	(signedIntOverflowIsUB and: [
		 aDRAdd type isIntegerType and: [ aDRAdd type isSigned ] ]) ifTrue: [
		| op1 op2 |
		op1 := self drValAsZ3Val: aDRAdd operand1 ifUnsupported: [ ^ nil ].
		op2 := self drValAsZ3Val: aDRAdd operand2 ifUnsupported: [ ^ nil ].
		assertions add: (Z3 mk_bvadd_no_overflow: ctxt _: op1 _: op2 _: true).
		assertions add: (Z3 mk_bvadd_no_underflow: ctxt _: op1 _: op2) ].

	^ self visitBinaryOp: aDRAdd with: #+
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBinaryOp: aDRInstruction with: selector [

	| constraints res op1 op2 |
	constraints := aDRInstruction operands do: [ :op |
		               self addZ3ConstraintFrom: op ].

	res := self drValAsZ3Val: aDRInstruction ifUnsupported: [ ^ nil ].
	op1 := self drValAsZ3Val: aDRInstruction operand1 ifUnsupported: [ ^ nil ].
	op2 := self drValAsZ3Val: aDRInstruction operand2 ifUnsupported: [ ^ nil ].

	self adaptSize: op1 with: op2 andDo: [ :op1 :op2 |
		assertions add: res === (op1 perform: selector with: op2) ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBitAnd: aDRBitAnd [

	^ self visitBinaryOp: aDRBitAnd with: #&
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitConstant: aDRConstantValue [

	aDRConstantValue value ifNil: [ ^ nil ].
	assertions add:
		(self drValAsZ3Val: aDRConstantValue ifUnsupported: [ ^ nil ])
		=== aDRConstantValue value
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitCopy: aDRCopy [
	| res op |
	
	aDRCopy dependencies do: [ :dep | self addZ3ConstraintFrom: dep ].

	res := self drValAsZ3Val: aDRCopy ifUnsupported: [ ^ nil ].
	op := self drValAsZ3Val: aDRCopy operand ifUnsupported: [ ^ nil ].

	assertions add: res === op.

	aDRCopy isPiNode ifTrue: [ assertions add: (aDRCopy asZ3Constraint: self) ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitGetFloatBits: aDRGetFloatBits [

	"TODO I think MachineArithmetic doesn't have floats yet? only Reals"
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitInterpreterReference: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLeftRotate: aDRRotateLeft [

	| constraints res op1 op2 |
	constraints := aDRRotateLeft operands do: [ :op |
		               self addZ3ConstraintFrom: op ].

	res := self drValAsZ3Val: aDRRotateLeft ifUnsupported: [ ^ nil ].
	op1 := self drValAsZ3Val: aDRRotateLeft operand1 ifUnsupported: [ ^ nil ].
	op2 := aDRRotateLeft operand2 simpleConstantFold.
	op2 isNumber ifFalse: [ ^nil ].

	assertions add: res === (op1 bitRotateLeft: op2)
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLeftShift: aDRLeftShift [

	^ self visitBinaryOp: aDRLeftShift with: #bitShiftLeft:
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoad: aDRLoad [

	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadArgument: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadReceiver: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitNullValue: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitPhiFunction: aDRPhiFunction [

	| res |
	aDRPhiFunction operands do: [ :op | self recurseOnlyPis: op ].

	res := self drValAsZ3Val: aDRPhiFunction ifUnsupported: [ ^ nil ].
	assertions add: ((aDRPhiFunction operands collect: [ :opDRVal |
			  | op |
			  op := self drValAsZ3Val: opDRVal ifUnsupported: [ ^ nil ].
			  self adaptSize: op with: res andDo: [ :_op :_res | _op === _res ] ])
			 reduce: [ :a :b | a or: b ])
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitSignedRightShift: aDRSignedRightShift [

	^ self visitBinaryOp: aDRSignedRightShift with: #bitShiftRightArithmetic:
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitSubstract: aDRSubtract [

	(signedIntOverflowIsUB and: [
		 aDRSubtract type isIntegerType and: [ aDRSubtract type isSigned ] ]) ifTrue: [
		| op1 op2 |
		op1 := self drValAsZ3Val: aDRSubtract operand1 ifUnsupported: [ ^ nil ].
		op2 := self drValAsZ3Val: aDRSubtract operand2 ifUnsupported: [ ^ nil ].
		assertions add: (Z3 mk_bvsub_no_overflow: ctxt _: op1 _: op2).
		assertions add: (Z3 mk_bvsub_no_underflow: ctxt _: op1 _: op2 _: true) ].

	^ self visitBinaryOp: aDRSubtract with: #-
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitUnsignedCoercion: aDRUnsignedCoercion [

	| res op |
	res := self drValAsZ3Val: aDRUnsignedCoercion ifUnsupported: [ ^ nil ].
	op := self drValAsZ3Val: aDRUnsignedCoercion operand1 ifUnsupported: [ ^ nil ].

	"Z3 bitvectors are unsigned, and you treat them as positive/negative when you operate on them"
	assertions add: res === op
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitUnsignedRightShift: aDRUnsignedRightShift [

	^ self visitBinaryOp: aDRUnsignedRightShift with: #bitShiftRight:
]
