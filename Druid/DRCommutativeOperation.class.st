Class {
	#name : #DRCommutativeOperation,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #converting }
DRCommutativeOperation >> reversed [

	^ self class operands: operands reversed result: result
]
