Class {
	#name : 'DRInterpreterPrimitiveObject',
	#superclass : 'DRInterpreterInstruction',
	#instVars : [
		'primitiveNumber'
	],
	#category : 'Druid-Cogit-Metacompiler',
	#package : 'Druid-Cogit',
	#tag : 'Metacompiler'
}

{ #category : 'accessing' }
DRInterpreterPrimitiveObject >> primitiveNumber [

	^ primitiveNumber
]

{ #category : 'accessing' }
DRInterpreterPrimitiveObject >> primitiveNumber: anInteger [ 

	primitiveNumber := anInteger 
]

{ #category : 'printing' }
DRInterpreterPrimitiveObject >> printOn: aStream [

	aStream
		<< 'Primitive: ';
		<< $#;
		<< self primitiveNumber asString;
		space;
		<< self sourceSelector
]
