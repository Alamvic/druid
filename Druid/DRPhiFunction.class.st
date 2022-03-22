Class {
	#name : #DRPhiFunction,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRPhiFunction class >> joinedVariables: aCollection [ 

	^ self new
		joinedVariables: aCollection;
		yourself
]

{ #category : #visiting }
DRPhiFunction >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitPhiFunction: self
]

{ #category : #accessing }
DRPhiFunction >> addValue: anInstruction [
	
	operands add: anInstruction.
	anInstruction addUser: self.
]

{ #category : #accessing }
DRPhiFunction >> addValue: anInstruction forPredecessor: aDRBasicBlock [ 
	
	self assert: self basicBlock predecessors last = aDRBasicBlock.
	operands addLast: anInstruction
]

{ #category : #accessing }
DRPhiFunction >> initialize [

	super initialize.
	operands := OrderedCollection new.
]

{ #category : #testing }
DRPhiFunction >> isEmpty [
	
	^ operands isEmpty 
]

{ #category : #testing }
DRPhiFunction >> isPhiFunction [

	^ true
]

{ #category : #accessing }
DRPhiFunction >> joinedVariables [
	
	^ self operands
]

{ #category : #accessing }
DRPhiFunction >> joinedVariables: aCollection [
	
	self operands: aCollection
]

{ #category : #accessing }
DRPhiFunction >> name [
	
	^ self result name
]

{ #category : #printing }
DRPhiFunction >> opcode [
	
	^ 'Ø'
	 
]

{ #category : #accessing }
DRPhiFunction >> result [
	
	^ result ifNil: [ self ]
]

{ #category : #'cogit-generation' }
DRPhiFunction >> rtlOperandQualifier [
	
	^ 'R:'
]

{ #category : #enumerating }
DRPhiFunction >> sccpLatticeValueFor: sccp [

	^ sccp latticeValueOfPhiFunction: self
]

{ #category : #simplification }
DRPhiFunction >> simplify [
	
	operands asSet size = 0 ifTrue: [ ^ DRNullValue new ].
	operands asSet size = 1 ifTrue: [ ^ operands first ].
	^ self
]

{ #category : #accessing }
DRPhiFunction >> singleChild [

	operands size > 1
		ifTrue: [ self error: 'Cannot simplify a phi function with more than one operand for now' ].
	^ operands first
]

{ #category : #accessing }
DRPhiFunction >> valuesForPredecessor: aDRBasicBlock [ 
	
	"Return a collection of all values in this Phi that correspond to the given basic block"
	"Automatic transformations could produce conditional branches that point to a single block in both branches"
	| values |
	values := OrderedCollection new.
	self basicBlock predecessors withIndexDo: [ :pred :index |
		pred = aDRBasicBlock ifTrue: [ values add: (operands at: index) ].
	].
	^ values
]
