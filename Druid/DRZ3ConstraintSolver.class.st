Class {
	#name : 'DRZ3ConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'visited',
		'cfg',
		'memo',
		'currentConstraint'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph [

	^ self new initializeFor: aDRControlFlowGraph
]

{ #category : 'initialization' }
DRZ3ConstraintSolver >> initializeFor: aDRControlFlowGraph [ 

	cfg := aDRControlFlowGraph.
	
	memo := IdentityDictionary new.
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> isSatisfiable: aDRPiNode [

	| operandConstraint newConstraint |
	currentConstraint := Bool true.

	operandConstraint := self z3ConstraintOn: aDRPiNode operand.
	newConstraint := self visitCopy: aDRPiNode.

	^ (currentConstraint ==> newConstraint not) isValid not
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitAddition: aDRAdd [ 

	^ self visitBinaryOp: aDRAdd with: #+.
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBinaryOp: aDRInstruction with: selector [

	aDRInstruction operands do: [ :op | self z3ConstraintOn: op ].

	^ aDRInstruction name toInt === (aDRInstruction operand1 name toInt
		   perform: selector
		   with: aDRInstruction operand2 name toInt)
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitCopy: aDRCopy [
	"TODO bitvectors? floats?"

	aDRCopy dependencies reject: #isNumberValue thenDo: [ :dep | self z3ConstraintOn: dep ].
	^ aDRCopy isPiNode
		  ifTrue: [ aDRCopy asZ3Constraint ]
		  ifFalse: [
			  | operand |
			  operand := aDRCopy operand.
			  aDRCopy name toInt === (operand isNumberValue
				   ifTrue: [ aDRCopy operand value toInt ]
				   ifFalse: [
					   operand name ]) ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
	^nil.
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> z3ConstraintOn: aDRValue [

	| newConstraint |
	"memo
		at: aDRValue
		ifPresent: [ :val |
		^ currentConstraint := currentConstraint and: val ].
	"
	newConstraint := (aDRValue acceptVisitor: self) ifNil: [ ^ currentConstraint ].
	memo at: aDRValue put: newConstraint.

	^ currentConstraint := currentConstraint and: newConstraint.

]
