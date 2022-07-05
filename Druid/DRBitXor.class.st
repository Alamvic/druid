Class {
	#name : #DRBitXor,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBitXor >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitXor: self
]

{ #category : #printing }
DRBitXor >> asNumberedExpression [

	| numberedExpression |
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression
		  operation: self class;
		  operands: self operands asBag;
		  yourself
]

{ #category : #printing }
DRBitXor >> opcode [
	
	^ 'BitXor'
]

{ #category : #printing }
DRBitXor >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand bitXor: operand2
]
