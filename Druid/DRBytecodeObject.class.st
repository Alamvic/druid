Class {
	#name : #DRBytecodeObject,
	#superclass : #DRDispatchTableObject,
	#instVars : [
		'bytecodeNumber',
		'isExtended'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #accessing }
DRBytecodeObject >> beExtended [
	" Set the receiver to be an extended bytecode "

	isExtended := true
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber [

	^ bytecodeNumber
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber: anObject [

	bytecodeNumber := anObject
]

{ #category : #initialization }
DRBytecodeObject >> initialize [ 

	super initialize.
	isExtended := false.
]

{ #category : #accessing }
DRBytecodeObject >> isExtended [

	^ isExtended
]

{ #category : #accessing }
DRBytecodeObject >> isExtended: anObject [

	isExtended := anObject
]
