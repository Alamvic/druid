Class {
	#name : 'DRBitXor',
	#superclass : 'DRCommutativeOperation',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBitXor >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitXor: self
]

{ #category : 'printing' }
DRBitXor >> asNumberedExpression [

	| numberedExpression |
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression
		  operation: self class;
		  operands: self operands asBag;
		  yourself
]

{ #category : 'printing' }
DRBitXor >> isBitXor [
	^ true
]

{ #category : 'printing' }
DRBitXor >> opcode [
	
	^ 'BitXor'
]

{ #category : 'printing' }
DRBitXor >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand bitXor: operand2
]
