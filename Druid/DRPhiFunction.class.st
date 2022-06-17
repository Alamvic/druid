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
DRPhiFunction >> addNext: anotherInstruction [
	
	^ basicBlock addInstructionFirst: anotherInstruction
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
	
	self haltIf: [ anInstruction isDRBlockClosure ].
	self assert: self basicBlock predecessors last = aDRBasicBlock.
	operands addLast: anInstruction
]

{ #category : #accessing }
DRPhiFunction >> initialize [

	super initialize.
	operands := OrderedCollection new.
]

{ #category : #operands }
DRPhiFunction >> isDominatedBy: anotherOperand [

	^ (super isDominatedBy: anotherOperand) or: [ 
		  anotherOperand basicBlock hasFrontier: self basicBlock ]
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
DRPhiFunction >> joinedVariables: aCollection [
	self haltIf: [ aCollection anySatisfy:  #isDRBlockClosure ].
	self operands: aCollection
]

{ #category : #operands }
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

	"If this is a recursive phi instruction, we do not care about the operands"
	anOperand == self ifTrue: [ ^ self ].

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

{ #category : #replace }
DRPhiFunction >> replaceOperandsIfDominated: myOperand withAll: newOperands [

	| fullNewOperands |
	"Replace phis operands is hard, we build a new one and replace"
	fullNewOperands := (operands copyWithout: myOperand) , newOperands.
	^ self replaceBy: (self basicBlock phiWithVariables: fullNewOperands)
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

{ #category : #operands }
DRPhiFunction >> selectOperand: anOperand with: anotherOperand [

	anOperand = anotherOperand ifTrue: [ ^ anOperand ].
	anOperand isNullValue ifTrue: [ ^ anotherOperand ].
	anotherOperand isNullValue ifTrue: [ ^ anOperand ].
	^ anotherOperand
]

{ #category : #simplification }
DRPhiFunction >> simplify [

	operands asSet size = 0 ifTrue: [ ^ DRNullValue new ].
	operands asSet size = 1 ifTrue: [ ^ operands first simplify ].
	^ self
]

{ #category : #simplification }
DRPhiFunction >> type [

	"Assume all my operands have the correct type"
	^ self operands anyOne type
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
