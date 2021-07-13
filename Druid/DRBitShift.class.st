Class {
	#name : #DRBitShift,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBitShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitShift: self
]

{ #category : #printing }
DRBitShift >> opcode [
	
	^ 'BitShift'
]

{ #category : #printing }
DRBitShift >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	op1 operand1 class = DRConstantValue ifTrue: [ 
		op2 operand1 class = DRConstantValue ifTrue: [ 
			self operands: (OrderedCollection new add: (DRConstantValue value: (op1 operand1 value) >> (op2 operand1 value)))]].
]
