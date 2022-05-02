Class {
	#name : #DRLiveSet,
	#superclass : #Object,
	#instVars : [
		'definition',
		'lastUsage',
		'location',
		'start',
		'liveRanges'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRLiveSet >> addLiveRanges: someLiveRanges [

	liveRanges addAll: someLiveRanges
]

{ #category : #ranges }
DRLiveSet >> addRangeFrom: firstInstruction to: lastInstruction [
	
	"Add a liveness range that goes from the first instruction to just before the last instruction"
	liveRanges add: (DRLiveRange new
		start: firstInstruction;
		stop: lastInstruction;
		yourself)
]

{ #category : #accessing }
DRLiveSet >> definition [
	^ definition
]

{ #category : #accessing }
DRLiveSet >> definition: aDefiningInstruction [ 

	definition := aDefiningInstruction
]

{ #category : #accessing }
DRLiveSet >> end [

	^ lastUsage
		  ifNil: [
				"No usages, then the instruction is live during its definition"
				self start ]
		  ifNotNil: [
				"Otherwise, it is live until
				- just after its last usage
			 	- but before the last usage defines a new value"
				lastUsage order - 1 ]
]

{ #category : #testing }
DRLiveSet >> initialize [
	super initialize.
	liveRanges := OrderedCollection new
]

{ #category : #testing }
DRLiveSet >> isActiveAt: position [

	^ self end >= position
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
DRLiveSet >> liveRanges [
	^ liveRanges
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
		  lastUsage: (second lastUsage ifNil: [ first lastUsage ]);
			addLiveRanges: first liveRanges;
			addLiveRanges: second liveRanges;
		  yourself
]

{ #category : #testing }
DRLiveSet >> overlaps: aDRLiveSet [ 
	
	^ liveRanges anySatisfy: [ :e | aDRLiveSet overlivesLiveRange: e ]
]

{ #category : #testing }
DRLiveSet >> overlivesLiveRange: aDRLiveRange [ 
	
	^ liveRanges anySatisfy: [ :e | e overlaps: aDRLiveRange ]
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
