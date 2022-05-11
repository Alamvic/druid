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

{ #category : #adding }
DRPhiFunction >> addOperand [

	operands add: DRNullValue new
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

{ #category : #'as yet unclassified' }
DRPhiFunction >> mergeOperands: anotherOperands [

	operands with: anotherOperands do: [ :op1 :op2 | 
		self replaceOperand: op1 by: (self selectOperand: op1 with: op2) ]
]

{ #category : #accessing }
DRPhiFunction >> name [
	
	^ self result name
]

{ #category : #printing }
DRPhiFunction >> opcode [
	
	^ 'Ø'
	 
]

{ #category : #'users-definitions' }
DRPhiFunction >> replaceOperand: anOperand by: anotherOperand [

	(anotherOperand isInstruction and: [anotherOperand basicBlock = self basicBlock])
		ifTrue: [ "Have no sense a phi with an block's instruction"
			anotherOperand isPhiFunction
				ifTrue: [ "unless it is another phi, then merge both"
					super replaceOperand: anOperand by: DRNullValue new.
					self mergeOperands: anotherOperand operands ]
				ifFalse: [ self error: 'Bad Phi operand' ] ]
		ifFalse: [ super replaceOperand: anOperand by: anotherOperand ]
]

{ #category : #'users-definitions' }
DRPhiFunction >> replaceOperandAtPredecessor: predecessor by: anotherOperand [

	| index |
	index := self basicBlock indexOfPredecessor: predecessor.
	self replaceOperandAtIndex: index by: anotherOperand
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

{ #category : #'as yet unclassified' }
DRPhiFunction >> selectOperand: anOperand with: anotherOperand [

	anOperand = anotherOperand ifTrue: [ ^ anOperand ].
	anOperand isNullValue ifTrue: [ ^ anotherOperand ].
	anotherOperand isNullValue ifTrue: [ ^ anOperand ].
	self error: 'Incompatible operands'
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
