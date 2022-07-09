Class {
	#name : #DRAdd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRAdd >> acceptVisitor: aVisitor [

	^ aVisitor visitAddition: self
]

{ #category : #printing }
DRAdd >> asNumberedExpression [

	| numberedExpression |
	
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression operation: self class;
	operands: self operands asBag;
	yourself
]

{ #category : #testing }
DRAdd >> isAdd [
	
	^ true
]

{ #category : #printing }
DRAdd >> opcode [
	
	^ 'ADD'
]

{ #category : #converting }
DRAdd >> reversed [

	^ self class operands: operands reversed result: result
]

{ #category : #printing }
DRAdd >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand + operand2
]

{ #category : #'constant folding' }
DRAdd >> simpleConstantFold [
	
	^ self
		sccpOperateOnLattice: self operand1 simpleConstantFold
		and: self operand2 simpleConstantFold
]

{ #category : #types }
DRAdd >> type [

	"Add is only valid among operands of the same type"

	(self operands collect: [ :e | e type ] as: Set) size = 1 ifFalse: [ 
		self error: 'incompatible types' ].
	
	^ self operands anyOne type
]
