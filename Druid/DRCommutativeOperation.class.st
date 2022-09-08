Class {
	#name : #DRCommutativeOperation,
	#superclass : #DRArithmeticOperation,
	#category : #'Druid-IR'
}

{ #category : #converting }
DRCommutativeOperation >> reversed [

	^ self class operands: operands reversed result: result
]
