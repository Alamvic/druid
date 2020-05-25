Class {
	#name : #DROneArgumentInstruction,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'Druid-instructions'
}

{ #category : #'instance creation' }
DROneArgumentInstruction class >> value: aValue [ 

	^ self new
		value: aValue;
		yourself
]

{ #category : #accessing }
DROneArgumentInstruction >> value: aValue [
	value := aValue
]
