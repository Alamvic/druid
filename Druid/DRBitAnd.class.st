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
DRBitAnd >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand bitAnd: operand2
]

{ #category : #printing }
DRBitAnd >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	((op1 isCopy) and: [op2 isCopy]) ifTrue: [ 
	op1 operand1 isConstant ifTrue: [ 
		op2 operand1 isConstant ifTrue: [ 
			^ DRCopy new operands: (DRConstantValue new value: (OrderedCollection new add: (op1 operand1 value & op2 operand1 value)))]]
	ifFalse: [ ^ self ]]. 
]
