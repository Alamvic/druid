Class {
	#name : #DRInterpreterValue,
	#superclass : #DRValue,
	#instVars : [
		'value'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRInterpreterValue class >> value: anInterpreterObject [

	^ self new
		value: anInterpreterObject;
		yourself
]

{ #category : #interpretation }
DRInterpreterValue >> lookupClass [
	
	^ value class
]

{ #category : #accessing }
DRInterpreterValue >> name [

	^ value class name
]

{ #category : #accessing }
DRInterpreterValue >> value [
	^ value
]

{ #category : #accessing }
DRInterpreterValue >> value: anInterpreterValue [ 
	value := anInterpreterValue
]
