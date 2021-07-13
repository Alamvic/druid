Class {
	#name : #DRBitAnd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBitAnd >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitAnd: self
]

{ #category : #printing }
DRBitAnd >> opcode [
	
	^ 'AND'
]

{ #category : #printing }
DRBitAnd >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	op1 operand1 class = DRConstantValue ifTrue: [ 
		op2 operand1 class = DRConstantValue ifTrue: [ 
			self operands: (OrderedCollection new add: (DRConstantValue value: (op1 operand1 value) & (op2 operand1 value)))]].
]
