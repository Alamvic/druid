Class {
	#name : #DRAdd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRAdd >> acceptVisitor: aVisitor [

	^ aVisitor visitAddition: self
]

{ #category : #testing }
DRAdd >> isAdd [
	
	^ true
]

{ #category : #printing }
DRAdd >> opcode [
	
	^ 'ADD'
]

{ #category : #printing }
DRAdd >> sccpLatticeValueFor: sccp [

	| operand1 operand2 |
	
	operand1 := sccp latticeValue: self operand1. 
	operand2 := sccp latticeValue: self operand2. 
	
	((({ sccp top. sccp bottom } includes: operand1) not
		and: [ ({ sccp top. sccp bottom } includes: operand2) not ])
			and: [ operand1 isConstant and: [ operand2 isConstant ]])
				ifTrue: [ ^ operand1 value + operand2 value ].
	(operand1 = sccp bottom or: [operand2 = sccp bottom])
		ifTrue: [ ^ sccp bottom ].
	^ sccp top
]

{ #category : #printing }
DRAdd >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	((op1 isCopy) and: [op2 isCopy]) ifTrue: 
	[ op1 operand1 isConstant ifTrue: [ 
		op2 operand1 isConstant ifTrue: [ 
			^ DRCopy new operands: (DRConstantValue new value: (OrderedCollection new add: (op1 operand1 value + op2 operand1 value)))]]
	ifFalse: [ ^ self ]]. 
]
