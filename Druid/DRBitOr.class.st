Class {
	#name : 'DRBitOr',
	#superclass : 'DRCommutativeOperation',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBitOr >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitOr: self
]

{ #category : 'printing' }
DRBitOr >> asNumberedExpression [

	| numberedExpression |
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression
		  operation: self class;
		  operands: self operands asBag;
		  yourself
]

{ #category : 'printing' }
DRBitOr >> isBitOr [ 
	^ true
]

{ #category : 'printing' }
DRBitOr >> opcode [
	
	^ 'BitOr'
]

{ #category : 'printing' }
DRBitOr >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand bitOr: operand2
]
