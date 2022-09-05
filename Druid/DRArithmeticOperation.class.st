Class {
	#name : #DRArithmeticOperation,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #converting }
DRArithmeticOperation >> reversed [

	^ self class operands: operands reversed result: result
]

{ #category : #type }
DRArithmeticOperation >> type [
	
	^ DRIntegerType new
]
