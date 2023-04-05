Class {
	#name : #DRInterpreterPrimitiveObject,
	#superclass : #DRInterpreterInstruction,
	#instVars : [
		'primitiveNumber'
	],
	#category : #'Druid-CompilerBuilder'
}

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
