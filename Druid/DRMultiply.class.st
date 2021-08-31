Class {
	#name : #DRMultiply,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRMultiply >> acceptVisitor: aVisitor [
	
	^ aVisitor visitMultiply: self
]

{ #category : #visiting }
DRMultiply >> sccpEvaluateFor: sccp [

	| left right |
	
	left := sccp value: self operand1. 
	right := sccp value: self operand2. 
	
	(left isInteger and: right isInteger) ifTrue: [ ^ left * right ].
	((left isInteger and: right = sccp top) or: (left = sccp top and: right isInteger)) ifTrue: [ ^ sccp top ]. 
	(left = sccp bottom or: right = sccp bottom) ifTrue: [ ^ sccp bottom ].
	(left = sccp top or: right = sccp top) ifTrue: [ ^ sccp top]. 
]

{ #category : #visiting }
DRMultiply >> tryConstantFolding [ 

	|op1 op2|
	
	op1 := self operand1.
	op2 := self operand2. 
	
	((op1 isCopy) and: [op2 isCopy]) ifTrue: [ 
	op1 operand1 isConstant ifTrue: [ 
		op2 operand1 isConstant ifTrue: [ 
				(op1 operand1 value = 0 or: op2 operand1 value = 0) ifTrue: [ ^ DRCopy new operands: (DRConstantValue new value: (OrderedCollection new add: (0))).
			]. 
			^ DRCopy new operands: (DRConstantValue new value: (OrderedCollection new add: (op1 operand1 value * op2 operand1 value)))]]
	ifFalse: [ ^ self ]]. 
]
