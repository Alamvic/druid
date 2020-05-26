Class {
	#name : #DROneArgumentInstruction,
	#superclass : #DRInstruction,
	#instVars : [
		'value'
	],
	#category : #'Druid-Instructions'
}

{ #category : #'instance creation' }
DROneArgumentInstruction class >> value: aValue [ 

	^ self new
		value: aValue;
		yourself
]

{ #category : #'as yet unclassified' }
DROneArgumentInstruction >> operands [

	^ { value }
]

{ #category : #accessing }
DROneArgumentInstruction >> value: aValue [
	value := aValue
]
