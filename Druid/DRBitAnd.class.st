Class {
	#name : #DRBitAnd,
	#superclass : #DRCommutativeOperation,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBitAnd >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitAnd: self
]

{ #category : #printing }
DRBitAnd >> asNumberedExpression [

	| numberedExpression |
	
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression operation: self class;
	operands: self operands asBag;
	yourself
]

{ #category : #printing }
DRBitAnd >> opcode [
	
	^ 'BitAnd'
]

{ #category : #printing }
DRBitAnd >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand bitAnd: operand2
]
