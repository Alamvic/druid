Class {
	#name : #DRConstantValue,
	#superclass : #DRValue,
	#instVars : [
		'value'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRConstantValue class >> value: anInteger [
	
	^ self new
		value: anInteger;
		yourself
]

{ #category : #testing }
DRConstantValue >> isConstant [
	
	^ true
]

{ #category : #interpretation }
DRConstantValue >> lookupClass [
	
	^ value class
]

{ #category : #accessing }
DRConstantValue >> name [
	
	^ value asString
]

{ #category : #'cogit-generation' }
DRConstantValue >> rtlOperandQualifier [
	
	^ 'Cq:'
]

{ #category : #accessing }
DRConstantValue >> value [
	^ value
]

{ #category : #accessing }
DRConstantValue >> value: anInteger [ 
	value := anInteger
]
