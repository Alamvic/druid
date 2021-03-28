Class {
	#name : #DRLiveSet,
	#superclass : #Object,
	#instVars : [
		'definition',
		'lastUsage',
		'register'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRLiveSet >> definition [
	^ definition
]

{ #category : #accessing }
DRLiveSet >> definition: aDefiningInstruction [ 
	definition := aDefiningInstruction
]

{ #category : #testing }
DRLiveSet >> isSpilled [
	
	^ false "yet"
]

{ #category : #accessing }
DRLiveSet >> lastUsage [
	^ lastUsage
]

{ #category : #accessing }
DRLiveSet >> lastUsage: anInstruction [ 
	lastUsage := anInstruction
]

{ #category : #printing }
DRLiveSet >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		print: definition result name;
		nextPut: $)
]

{ #category : #accessing }
DRLiveSet >> register [
	^ register
]

{ #category : #accessing }
DRLiveSet >> register: aPhisicalRegister [
	
	register := aPhisicalRegister
]
