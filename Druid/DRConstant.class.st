Class {
	#name : #DRConstant,
	#superclass : #DRValue,
	#instVars : [
		'value'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRConstant class >> value: anInteger [
	
	^ self new
		value: anInteger;
		yourself
]

{ #category : #testing }
DRConstant >> isConstant [
	
	^ true
]

{ #category : #interpretation }
DRConstant >> lookupClass [
	
	^ value class
]

{ #category : #'cogit-generation' }
DRConstant >> rtlOperandQualifier [
	
	^ 'Cq:'
]

{ #category : #accessing }
DRConstant >> value [
	^ value
]

{ #category : #accessing }
DRConstant >> value: anInteger [ 
	value := anInteger
]
