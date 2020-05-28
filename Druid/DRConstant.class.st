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

{ #category : #printing }
DRConstant >> printOn: aStream [

	^ aStream nextPutAll: self value asString
]

{ #category : #accessing }
DRConstant >> registers [
	^ {}
]

{ #category : #'register allocation' }
DRConstant >> updateLogicalRegisters: mappings [ 
	
	^ self
]

{ #category : #accessing }
DRConstant >> value [
	^ value
]

{ #category : #accessing }
DRConstant >> value: anObject [
	value := anObject
]
