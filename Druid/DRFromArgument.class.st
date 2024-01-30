Class {
	#name : #DRFromArgument,
	#superclass : #Object,
	#instVars : [
		'index'
	],
	#category : #'Druid-BytecodeGeneration-Deoptimization'
}

{ #category : #accessing }
DRFromArgument >> index: anInteger [ 
	index := anInteger
]

{ #category : #reading }
DRFromArgument >> readFromContext: aContext [ 
	
	^ aContext arguments at: index + 1 
]
