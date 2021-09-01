Class {
	#name : #DRSubtract,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRSubtract >> acceptVisitor: aVisitor [
	
	^ aVisitor visitSubstract: self
]

{ #category : #printing }
DRSubtract >> opcode [
	
	^ 'SUB'
]

{ #category : #printing }
DRSubtract >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand - operand2
]

{ #category : #printing }
DRSubtract >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	((op1 isCopy) and: [op2 isCopy]) ifTrue: [ 
	op1 operand1 isConstant ifTrue: [ 
		op2 operand1 isConstant ifTrue: [ 
			^ DRCopy new operands: (DRConstantValue new value: (OrderedCollection new add: (op1 operand1 value - op2 operand1 value)))]]
	ifFalse: [ ^ self ]]. 
]
