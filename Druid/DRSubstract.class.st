Class {
	#name : #DRSubstract,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRSubstract >> acceptVisitor: aVisitor [
	
	^ aVisitor visitSubstract: self
]

{ #category : #printing }
DRSubstract >> opcode [
	
	^ 'SUB'
]

{ #category : #printing }
DRSubstract >> sccpEvaluateFor: sccp [
	
	| operand1 operand2 |
	operand1 := sccp value: self operand1.
	operand2 := sccp value: self operand2.
	
	((({ sccp top. sccp bottom } includes: operand1) not
		and: [ ({ sccp top. sccp bottom } includes: operand2) not ])
			and: [ operand1 isConstant and: [ operand2 isConstant ]])
				ifTrue: [ ^ operand1 value - operand2 value ].
	
	operand1 = sccp bottom ifTrue: [ ^ sccp bottom ].
	operand2 = sccp bottom ifTrue: [ ^ sccp bottom ].
	
	^ sccp top
]

{ #category : #printing }
DRSubstract >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	((op1 isCopy) and: [op2 isCopy]) ifTrue: [ 
	op1 operand1 isConstant ifTrue: [ 
		op2 operand1 isConstant ifTrue: [ 
			^ DRCopy new operands: (DRConstantValue new value: (OrderedCollection new add: (op1 operand1 value - op2 operand1 value)))]]
	ifFalse: [ ^ self ]]. 
]
