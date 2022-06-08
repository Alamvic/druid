Class {
	#name : #DRLiveSet,
	#superclass : #Object,
	#instVars : [
		'location',
		'liveRanges'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #ranges }
DRLiveSet >> addLiveRange: aRange [
	
	"Add a liveness range that goes from the first instruction to just before the last instruction"
	liveRanges add: aRange
]

{ #category : #accessing }
DRLiveSet >> addLiveRanges: someLiveRanges [

	someLiveRanges do: [ :e | self addLiveRange: e ]
]

{ #category : #ranges }
DRLiveSet >> addRangeFrom: firstInstruction to: lastInstruction [

	"Add a liveness range that goes from the first instruction to just before the last instruction"

	self addLiveRange: (DRLiveRange new
			 start: firstInstruction;
			 stop: lastInstruction;
			 yourself).
]

{ #category : #testing }
DRLiveSet >> initialize [
	super initialize.
	liveRanges := OrderedCollection new
]

{ #category : #testing }
DRLiveSet >> isActiveAt: position [

	^ liveRanges anySatisfy: [ :e | e isActiveAt: position ]
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
	^ DRLiveSet new
		  addLiveRanges: self liveRanges;
		  addLiveRanges: aDRLiveSet liveRanges;
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
	aStream << '['.
	aStream << self start asString.
	aStream << ', '.
	aStream << self stop asString.
	aStream << '['.
]

{ #category : #printing }
DRLiveSet >> size [

	^ self stop - self start
]

{ #category : #accessing }
DRLiveSet >> start [
	
	^ liveRanges min: [ :e | e start order ]
]

{ #category : #accessing }
DRLiveSet >> stop [
	
	^ liveRanges max: [ :e | e stop order ]
]
