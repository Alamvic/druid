Class {
	#name : #DRConstant,
	#superclass : #DRValue,
	#instVars : [
		'value'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRConstant class >> value: aValue [

	^ self new
		value: aValue;
		yourself
]

{ #category : #testing }
DRConstant >> isConstant [
	
	^ true
]

{ #category : #testing }
DRConstant >> isConstantInteger [
	
	^ value isInteger
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
