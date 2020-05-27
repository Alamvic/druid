Class {
	#name : #DROneArgumentInstruction,
	#superclass : #DRInstruction,
	#instVars : [
		'valueInstruction',
		'valueRegister'
	],
	#category : #'Druid-Instructions'
}

{ #category : #'instance creation' }
DROneArgumentInstruction class >> value: aValue [

	^ self value: aValue result: nil
]

{ #category : #'instance creation' }
DROneArgumentInstruction class >> value: aValue result: aResult [

	^ self new
		value: aValue;
		result: aResult
		yourself
]

{ #category : #'as yet unclassified' }
DROneArgumentInstruction >> operands [

	^ { valueRegister }
]

{ #category : #accessing }
DROneArgumentInstruction >> value [
	
	^ valueRegister
]

{ #category : #accessing }
DROneArgumentInstruction >> value: aValue [

	valueInstruction := aValue.
	valueRegister := aValue result
]
