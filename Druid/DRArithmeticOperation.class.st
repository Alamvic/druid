Class {
	#name : #DRArithmeticOperation,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #type }
DRArithmeticOperation >> type [
	
	^ DRIntegerType new
]
