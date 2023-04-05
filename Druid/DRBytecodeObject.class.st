Class {
	#name : #DRBytecodeObject,
	#superclass : #DRInterpreterInstruction,
	#instVars : [
		'bytecodeNumber',
		'bytecodeSize'
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

{ #category : #accessing }
DRBytecodeObject >> bytecodeSize [

	^ bytecodeSize
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeSize: anObject [

	bytecodeSize := anObject
]
