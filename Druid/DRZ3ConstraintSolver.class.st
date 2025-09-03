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

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> addZ3ConstraintFrom: aDRValue [

	aDRValue acceptVisitor: self
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> drValAsZ3Val: aDRValue [

	aDRValue type isIntegerType ifTrue: [
		^ aDRValue name toBitVector: aDRValue type numberOfBits ].

	aDRValue type isBooleanType ifTrue: [ ^ aDRValue name toBool ].

	aDRValue type isFloatType ifTrue: [ ^ aDRValue name toReal ].
	
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
	^ retval
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> recurseOnlyPis: aDRValue [

	| currentVal |
	currentVal := aDRValue.
	[ currentVal isPiNode and: [ currentVal operand isPhiFunction not ] ]
		whileTrue: [
			| res op |
			res := self drValAsZ3Val: currentVal.
			op := self drValAsZ3Val: currentVal operand.

			assertions add: (res === op and: (currentVal asZ3Constraint: self)).
			self recurseOnlyPis: currentVal constraint otherValue.
			currentVal := currentVal operand ].

	currentVal := currentVal simpleConstantFold asDRValue.
	currentVal isNumberValue ifTrue: [
		assertions add: (self drValAsZ3Val: aDRValue) === currentVal value ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitAddition: aDRAdd [

	(signedIntOverflowIsUB and: [
		 aDRAdd type isIntegerType and: [ aDRAdd type isSigned ] ]) ifTrue: [
		| op1 op2 |
		op1 := self drValAsZ3Val: aDRAdd operand1.
		op2 := self drValAsZ3Val: aDRAdd operand2.
		assertions add: (Z3 mk_bvadd_no_overflow: ctxt _: op1 _: op2 _: true).
		assertions add: (Z3 mk_bvadd_no_underflow: ctxt _: op1 _: op2) ].

	^ self visitBinaryOp: aDRAdd with: #+
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBinaryOp: aDRInstruction with: selector [

	| constraints res op1 op2 |
	constraints := aDRInstruction operands do: [ :op |
		               self addZ3ConstraintFrom: op ].

	res := self drValAsZ3Val: aDRInstruction.
	op1 := self drValAsZ3Val: aDRInstruction operand1.
	op2 := self drValAsZ3Val: aDRInstruction operand2.

	assertions add: res === (op1 perform: selector with: op2)
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitConstant: aDRConstantValue [

	assertions add:
		(self drValAsZ3Val: aDRConstantValue) === aDRConstantValue value
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitCopy: aDRCopy [
	"TODO bitvectors? floats?"

	| res op |
	aDRCopy dependencies do: [ :dep | self addZ3ConstraintFrom: dep ].

	res := self drValAsZ3Val: aDRCopy.
	op := self drValAsZ3Val: aDRCopy operand.

	assertions add: res === op.

	aDRCopy isPiNode ifTrue: [ assertions add: (aDRCopy asZ3Constraint: self) ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoad: aDRLoad [

	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitPhiFunction: aDRPhiFunction [

	| res |
	aDRPhiFunction operands do: [ :op | self recurseOnlyPis: op ].

	res := self drValAsZ3Val: aDRPhiFunction.
	assertions add: ((aDRPhiFunction operands collect: [ :opDRVal |
			  (self drValAsZ3Val: opDRVal) === res ]) reduce: [ :a :b | a or: b ])
]
