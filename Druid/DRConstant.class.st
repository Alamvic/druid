Class {
	#name : #DRConstant,
	#superclass : #DRInstruction,
	#instVars : [
		'value'
	],
	#category : #'Druid-instructions'
}

{ #category : #accessing }
DRConstant class >> value: aValue [

	^ self new
		value: aValue;
		yourself
]

{ #category : #'as yet unclassified' }
DRConstant >> appendOn: anInstructionBuilder [

	"Do nothing. Constants are not appended. Only on access"
]

{ #category : #accessing }
DRConstant >> result [
	
	^ value
]

{ #category : #accessing }
DRConstant >> value [
	^ value
]

{ #category : #accessing }
DRConstant >> value: anObject [
	value := anObject
]
