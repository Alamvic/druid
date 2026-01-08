Class {
	#name : 'DRInterpreterValue',
	#superclass : 'DRValue',
	#instVars : [
		'value'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRInterpreterValue class >> value: anInterpreterObject [

	^ self new
		  value: anInterpreterObject value;
		  yourself
]

{ #category : 'visiting' }
DRInterpreterValue >> acceptVisitor: aVisitor [

	^ aVisitor visitInterpreterValue: self
]

{ #category : 'lookup' }
DRInterpreterValue >> astForSelector: aString [

	^ (self lookupSelector: aString) ast
]

{ #category : 'testing' }
DRInterpreterValue >> isFloatType [

	^ false
]

{ #category : 'testing' }
DRInterpreterValue >> isInterpreterValue [

	^ true
]

{ #category : 'testing' }
DRInterpreterValue >> isUndefined [

	^ false
]

{ #category : 'interpretation' }
DRInterpreterValue >> lookupClass [
	
	^ value class
]

{ #category : 'lookup' }
DRInterpreterValue >> lookupSelector: aString [ 

	^ value class lookupSelector: aString
]

{ #category : 'accessing' }
DRInterpreterValue >> name [

	^ value class name
]

{ #category : 'interpretation' }
DRInterpreterValue >> typeWithAlreadySeen: aCollection [
	
	^ self
]

{ #category : 'accessing' }
DRInterpreterValue >> value [
	^ value
]

{ #category : 'accessing' }
DRInterpreterValue >> value: anInterpreterValue [ 
	value := anInterpreterValue
]
