Class {
	#name : #DRIndirection,
	#superclass : #DRValue,
	#instVars : [
		'value'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRIndirection class >> value: aValue [

	^ self new
		value: aValue;
		yourself
]

{ #category : #accessing }
DRIndirection >> result [

	^ '(', self value result asString, ')'
]

{ #category : #accessing }
DRIndirection >> value [
	^ value
]

{ #category : #accessing }
DRIndirection >> value: anObject [
	value := anObject
]
