Class {
	#name : #DRBytecodeObject,
	#superclass : #DRDispatchTableObject,
	#instVars : [
		'bytecodeNumber'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber [

	^ bytecodeNumber
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber: anObject [

	bytecodeNumber := anObject
]
