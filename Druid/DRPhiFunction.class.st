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
	
	self assert: self basicBlock predecessors last = aDRBasicBlock.
	operands addLast: anInstruction
]

{ #category : #converting }
DRPhiFunction >> asJitCompileTimeExpression [
	
	"If staged, a phi function has the same value as its result.
	All transformations should have been done by the staging and the register allocator"
	
	^ result name asDRValue asJitCompileTimeExpression  
]

{ #category : #'loop-invariance' }
DRPhiFunction >> canMoveOutsideLoop: backJump [

	^ false
]

{ #category : #accessing }
DRPhiFunction >> conditionalBranch [

	| dominator conditionalBranch |
	dominator := self controlFlowGraph dominatorTree dominatorOfAll: self operands.
	conditionalBranch := dominator block lastInstruction.
	self assert: conditionalBranch isConditionalBranch.
	^ conditionalBranch
]

{ #category : #testing }
DRPhiFunction >> hasRecursiveUse [

	self recursiveUsersDo: [ :i | i = self ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #testing }
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

{ #category : #'loop-invariance' }
DRPhiFunction >> isInvariantFrom: loop [

	"This is thought for phis inside loop header blocks. 
	The invariance from the loop body is correct if there are no changes for the variable inside the loop => the value for that predecessor is null"

	^ (self valuesForPredecessor: loop latchBlock) anySatisfy: #isNullValue.
]

{ #category : #testing }
DRPhiFunction >> isJITCompileTimeExpression [
	
	^ self operands allSatisfy: [ :op | op isJITCompileTimeExpressionIgnoring: { self } ]
]

{ #category : #testing }
DRPhiFunction >> isJITCompileTimeExpressionIgnoring: aCollection [

	(aCollection includes: self) ifTrue: [ ^ true "?" ].

	^ self operands allSatisfy: [ :op |
		  op isJITCompileTimeExpressionIgnoring: aCollection , { self } ]
]

{ #category : #testing }
DRPhiFunction >> isPhiFunction [

	^ true
]

{ #category : #accessing }
DRPhiFunction >> joinedVariables: aCollection [

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

{ #category : #accessing }
DRPhiFunction >> predecessorAtOperand: anOperand [

	| index |
	index := operands indexOf: anOperand.
	^ self basicBlock predecessorAt: index
]

{ #category : #'users-definitions' }
DRPhiFunction >> replaceOperand: anOperand by: anotherOperand [

	"If this is a recursive phi instruction, we do not care about the operands"
	anOperand == self ifTrue: [ ^ self ].
	super replaceOperand: anOperand by: anotherOperand
]

{ #category : #'users-definitions' }
DRPhiFunction >> replaceOperandAtPredecessor: predecessor by: anotherOperand [

	| index |
	index := self basicBlock indexOfPredecessor: predecessor.
	self replaceOperandAtIndex: index by: anotherOperand
]

{ #category : #replace }
DRPhiFunction >> replaceOperandsIfDominated: myOperand withAll: newOperands [

	| replacingOperands |

	replacingOperands := operands collectWithIndex: [ :op :i | 
		op = myOperand 
		ifTrue: [ | predecessor | "Find the new operand dominator of the predecessor"
			predecessor := self basicBlock predecessorAt: i.
			newOperands detect: [ :newOperand | predecessor isDominatedBy: newOperand basicBlock ] ifNone: [ ^ nil ] ] 
		ifFalse: [ op ] 
	].
	
	"Replace phis operands is hard, we build a new one and replace"	
	^ self replaceBy: (self basicBlock phiWithVariables: replacingOperands)
]

{ #category : #accessing }
DRPhiFunction >> result [
	
	^ result ifNil: [ self ]
]

{ #category : #'loop-invariance' }
DRPhiFunction >> rewirePhiTo: destinationBlock [

	| newOperandValues phiReplacement |
	newOperandValues := self valuesForPredecessor: destinationBlock.
	self assert: [ newOperandValues size = 1 ].
	phiReplacement := self instructionFactory copy: newOperandValues anyOne.
	self replaceBy: phiReplacement
]

{ #category : #SCCP }
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

{ #category : #'constant folding' }
DRPhiFunction >> simpleConstantFold [

	| operandsWithoutMe operandValues |
	operandsWithoutMe := self operands copyWithout: self.
	operandsWithoutMe size = 1 ifTrue: [ ^ operandsWithoutMe anyOne simpleConstantFold ].
	(self hasRecursiveUse) ifTrue: [ ^ super simpleConstantFold ].
	operandValues := operandsWithoutMe collect: [ :e | e simpleConstantFold ] as: Set.
	operandValues size = 1 ifTrue: [ ^ operandValues anyOne ].
	^ super simpleConstantFold
]

{ #category : #transforming }
DRPhiFunction >> simplify [

	| simplifiedOperands |
	operands asSet isEmpty ifTrue: [ ^ DRNullValue new ].

	"Use a worklist algorithm to iterate over the phi graph.
	Ignore Phis and cut if we see a duplicated phi, this may happen in loops"
	simplifiedOperands := Set new.
	(OrderedCollection withAll: operands) whileNotEmpty: [:next :col :marked | 
		(marked includes: next) ifFalse: [
			next isPhiFunction
				ifTrue: [ 
					marked add: next.
					col addAll: next operands ]
				ifFalse: [ simplifiedOperands add: next ] ]  ].
	simplifiedOperands size = 1 ifTrue: [ ^ simplifiedOperands anyOne ].
	^ self
]

{ #category : #testing }
DRPhiFunction >> supportConstantOperand [

	^ false
]

{ #category : #types }
DRPhiFunction >> type [

	(self operands copyWithout: self) do: [ :o |
		| operandType |
		operandType := o type.
		operandType isUndefined ifFalse: [ ^ operandType ] ].

	^ DRUndefinedType new
]

{ #category : #simplification }
DRPhiFunction >> typeWithAlreadySeen: aCollection [

	(aCollection includes: self)
		ifTrue: [ ^ DRUndefinedType new ].
	
	aCollection add: self.
	self operands do: [ :e | | operandType |
		operandType := e typeWithAlreadySeen: aCollection.
		"If nil it means it's a recursive type, we continue with the next operand"
		operandType ifNotNil: [ ^ operandType ] ].
	
	self halt
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
