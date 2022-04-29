Class {
	#name : #DRLiveSet,
	#superclass : #Object,
	#instVars : [
		'definition',
		'lastUsage',
		'location',
		'start'
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

{ #category : #accessing }
DRLiveSet >> definitionOrder [
	
	^ definition order
]

{ #category : #testing }
DRLiveSet >> isActiveAt: position [

	^ self lastUsage notNil and: [ self lastUsage order > position ]
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

{ #category : #merging }
DRLiveSet >> mergeWith: aDRLiveSet [

	"Merge two non overlapping live sets"

	| first second |
	self definition order < aDRLiveSet definition order
		ifTrue: [ 
			first := self.
			second := aDRLiveSet ]
		ifFalse: [ 
			second := self.
			first := aDRLiveSet ].

	^ DRLiveSet new
		  definition: first definition;
		  lastUsage: second lastUsage;
		  yourself
]

{ #category : #accessing }
DRLiveSet >> orderInterval [

	| end |
	end := lastUsage ifNil: [
			"No usages, then the instruction is live during its definition" 
			definition order + 1 ]
		ifNotNil: [
			"Otherwise, it is live until 
			 - just after its last usage
			 - but before the last usage defines a new value"
			lastUsage order - 1 ].
	^ definition order to: end
]

{ #category : #testing }
DRLiveSet >> overlaps: aDRLiveSet [ 
	
	^ (self orderInterval intersection: aDRLiveSet orderInterval) notEmpty
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
DRLiveSet >> start [
	
	^ start ifNil: [definition order]
]

{ #category : #accessing }
DRLiveSet >> start: aPosition [

	start := aPosition
]
