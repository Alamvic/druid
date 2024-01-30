Class {
	#name : #DRFromConstantValue,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'Druid-BytecodeGeneration-Deoptimization'
}

{ #category : #reading }
DRFromConstantValue >> readFromContext: aContext [ 
	
	^ value
]

{ #category : #accessing }
DRFromConstantValue >> value: aValue [ 
	value := aValue
]
