Class {
	#name : #DRLiveSet,
	#superclass : #Object,
	#instVars : [
		'definition',
		'lastUsage',
		'location'
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
	
	^ location isMemoryAddress
]

{ #category : #accessing }
DRLiveSet >> lastUsage [
	^ lastUsage
]

{ #category : #accessing }
DRLiveSet >> lastUsage: anInstruction [ 
	lastUsage := anInstruction
]

{ #category : #accessing }
DRLiveSet >> location [
	^ location
]

{ #category : #accessing }
DRLiveSet >> location: aLocation [
	
	location := aLocation
]

{ #category : #printing }
DRLiveSet >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		print: definition result name;
		nextPut: $)
]
