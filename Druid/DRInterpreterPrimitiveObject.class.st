Class {
	#name : #DRInterpreterPrimitiveObject,
	#superclass : #Object,
	#instVars : [
		'sourceMethod',
		'primitiveNumber'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #testing }
DRInterpreterPrimitiveObject >> hasPragmaNamed: aString [

	^ self sourceMethod hasPragmaNamed: aString
]

{ #category : #accessing }
DRInterpreterPrimitiveObject >> primitiveNumber [

	^ primitiveNumber
]

{ #category : #accessing }
DRInterpreterPrimitiveObject >> primitiveNumber: anInteger [ 

	primitiveNumber := anInteger 
]

{ #category : #printing }
DRInterpreterPrimitiveObject >> printOn: aStream [

	aStream
		<< $(
		<< self primitiveNumber asString;
		<< $);
		space;
		<< self sourceSelector
]

{ #category : #accessing }
DRInterpreterPrimitiveObject >> sourceMethod [

	^ sourceMethod
]

{ #category : #accessing }
DRInterpreterPrimitiveObject >> sourceMethod: anObject [

	sourceMethod := anObject
]

{ #category : #accessing }
DRInterpreterPrimitiveObject >> sourceSelector [

	^ self sourceMethod selector
]
