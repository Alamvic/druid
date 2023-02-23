Class {
	#name : #DRAdd,
	#superclass : #DRCommutativeOperation,
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

{ #category : #printing }
DRAdd >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand + operand2
]
