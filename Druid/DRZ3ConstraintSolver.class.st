Class {
	#name : 'DRZ3ConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'cfg',
		'memo',
		'assertions',
		'ctxt',
		'signedIntOverflowIsUB',
		'phiReplacement',
		'freeVarCounter'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph [

	^ self for: aDRControlFlowGraph signedIntOverflowIsUB: false
]

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean [

	^ self new initializeFor: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean
]

{ #category : 'as yet unclassified' }
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

	| converter |
	converter := self
		             getZ3ValueConverterFor: aDRValue type
		             ifUnsupported: aBlock.
	
	^ converter value: (aDRValue isConstant
			   ifTrue: [ aDRValue value ]
			   ifFalse: [ aDRValue name ])
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> getZ3ValueConverterFor: type ifUnsupported: aBlock [

	type isIntegerType ifTrue: [
		^ [ :val | val toBitVector: type numberOfBits ] ].

	type isBooleanType ifTrue: [ ^ [ :val | val toBool ] ].

	type isFloatType ifTrue: [ "TODO MachineArithmetic doesn't have floats yet? only Reals" ].

	(type isUndefined or: [ type isFloatType ]) ifTrue: [ ^ aBlock value ].

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
	freeVarCounter := 0.

	ctxt := Z3Context global.
	solver := ctxt mkSolver.

	self addZ3ConstraintFrom: aDRPiNode.

	assertions do: [ :a | solver assert: a ].
	retval := solver check.

	solver release.
	^ retval ifNil: [ true ]
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> isSatisfiable: aDRPiNode replacingPhi: phi for: phiOp [

	| retval |
	phiReplacement := phi -> phiOp.
	retval := self isSatisfiable: aDRPiNode.
	phiReplacement := nil.

	^ retval
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> nextFreeVarOfType: type ifUnsupported: aBlock [

	| varName converter |
	converter := self getZ3ValueConverterFor: type ifUnsupported: aBlock.
	
	freeVarCounter := freeVarCounter + 1.
	varName := 'z3tmp_' , freeVarCounter asString.
	
	^ converter value: varName.
	
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> recurseOnlyPis: aDRValue [

	| currentVal varForConstraints |
	currentVal := aDRValue.

	varForConstraints := self
		                     nextFreeVarOfType: aDRValue type
		                     ifUnsupported: [ ^ nil ].

	[ currentVal isPiNode and: [ currentVal operand isPhiFunction not ] ]
		whileTrue: [
			assertions add:
				(currentVal constraint asZ3ConstraintOnVar: varForConstraints).
			currentVal := currentVal operand ].

	(currentVal isCopy or: [ currentVal isConstant ]) ifFalse: [ ^ varForConstraints ].

	currentVal := currentVal simpleConstantFold asDRValue.
	currentVal isConstant ifTrue: [
		assertions add: varForConstraints === currentVal value ].

	^ varForConstraints
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

	| res newVars |

	res := self drValAsZ3Val: aDRPhiFunction ifUnsupported: [ ^ nil ].

	phiReplacement ifNotNil: [
		phiReplacement key = aDRPhiFunction ifTrue: [ |replacement replacementZ3|
			replacement := phiReplacement value.
			replacementZ3 := self drValAsZ3Val: replacement ifUnsupported: [ ^nil ].
			self addZ3ConstraintFrom: replacement.
			assertions add: res === replacementZ3.
			^ nil
		]
	].

	newVars := aDRPhiFunction operands collect: [ :op | self recurseOnlyPis: op ].

	assertions add: ((newVars collect: [ :op |
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
