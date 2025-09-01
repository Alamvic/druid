Class {
	#name : 'DRZ3ConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'cfg',
		'memo',
		'assertions'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph [

	^ self new initializeFor: aDRControlFlowGraph
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> addZ3ConstraintFrom: aDRValue [

	aDRValue acceptVisitor: self
]

{ #category : 'initialization' }
DRZ3ConstraintSolver >> initializeFor: aDRControlFlowGraph [ 

	cfg := aDRControlFlowGraph.
	
	memo := IdentityDictionary new.
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> isSatisfiable: aDRPiNode [

	| solver ctxt retval |
	
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
	[ currentVal isPiNode and: [ currentVal operand isPhiFunction not ] ] whileTrue: [
		assertions add: (currentVal name toInt === currentVal operand name and:
				 currentVal asZ3Constraint).
		self recurseOnlyPis: currentVal constraint otherValue.
		currentVal := currentVal operand ].

	currentVal := currentVal simpleConstantFold asDRValue.
	currentVal isNumberValue ifTrue: [
		assertions add: aDRValue name toInt === currentVal value ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitAddition: aDRAdd [ 

	^ self visitBinaryOp: aDRAdd with: #+.
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBinaryOp: aDRInstruction with: selector [

	| constraints |
	constraints := aDRInstruction operands do: [ :op |
		               self addZ3ConstraintFrom: op ].
	assertions add:
		aDRInstruction name toInt === (aDRInstruction operand1 name toInt
			 perform: selector
			 with: aDRInstruction operand2 name toInt)
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitConstant: aDRConstantValue [

	assertions add:
		aDRConstantValue name toInt === aDRConstantValue value
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitCopy: aDRCopy [
	"TODO bitvectors? floats?"

	aDRCopy dependencies
		reject: #isNumberValue
		thenDo: [ :dep | self addZ3ConstraintFrom: dep ].

	assertions add:
		aDRCopy name toInt === (aDRCopy operand isNumberValue
			 ifTrue: [ aDRCopy operand value toInt ]
			 ifFalse: [ aDRCopy operand name ]).

	aDRCopy isPiNode ifTrue: [
		assertions add: aDRCopy asZ3Constraint.
	].

	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitPhiFunction: aDRPhiFunction [

	aDRPhiFunction operands do: [ :op | self recurseOnlyPis: op ].
	assertions add: ((aDRPhiFunction operands collect: [ :op |
			  op name toInt === aDRPhiFunction name toInt ]) reduce: [ :a :b |
			 a or: b ]).

]
