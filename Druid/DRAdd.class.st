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
DRAdd >> opcode [
	
	^ 'ADD'
]

{ #category : #printing }
DRAdd >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	op1 operand1 class = DRConstantValue ifTrue: [ 
		op2 operand1 class = DRConstantValue ifTrue: [ 
			self operands: (OrderedCollection new add: (DRConstantValue value: (op1 operand1 value) + (op2 operand1 value)))]].
]
