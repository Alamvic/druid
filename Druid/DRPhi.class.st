Class {
	#name : #DRPhi,
	#superclass : #DRInstruction,
	#instVars : [
		'values'
	],
	#category : #'Druid-Instructions'
}

{ #category : #printing }
DRPhi >> mnemonic [
	
	^ 'PHI'
]

{ #category : #'as yet unclassified' }
DRPhi >> operands [
	
	^ values
]

{ #category : #printing }
DRPhi >> printOn: aStream [

	aStream
		nextPutAll: 'PHI(';
		print: self values asString;
		nextPutAll: ' -> ';
		print: self result;
		nextPutAll: ')'
]

{ #category : #accessing }
DRPhi >> values [
	^ values
]

{ #category : #accessing }
DRPhi >> values: anObject [
	values := anObject
]
