Class {
	#name : 'DRBitAnd',
	#superclass : 'DRCommutativeOperation',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBitAnd >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitAnd: self
]

{ #category : 'printing' }
DRBitAnd >> asNumberedExpression [

	| numberedExpression |
	
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression operation: self class;
	operands: self operands asBag;
	yourself
]

{ #category : 'types' }
DRBitAnd >> isBitAnd [

	^ true
]

{ #category : 'printing' }
DRBitAnd >> opcode [
	
	^ 'BitAnd'
]

{ #category : 'printing' }
DRBitAnd >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand bitAnd: operand2
]

{ #category : 'types' }
DRBitAnd >> typeWithAlreadySeen: aCollection [

	^ DRUnsignedIntegerType size: 8 "bytes per word"
]
