Class {
	#name : #DRCommutativeOperation,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRCommutativeOperation >> hasSameExpressionAs: instruction [

	self class = instruction class ifTrue: [
		(self operands includesAll: instruction operands) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #converting }
DRCommutativeOperation >> reversed [

	^ self class operands: operands reversed result: result
]
