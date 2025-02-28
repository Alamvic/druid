"
A LiveSet represents the set of live variables at a particular point in the program's code. It keeps track of the variables that are still being used or modified within a specific interval or range (see also `DRLiveRange`). The LiveSet is typically associated with a specific program point, such as a basic block or an instruction.

This class has methods to manipulate and query the set of live variables. 

## Important methods

- The `overlaps:` method takes another `DRLiveSet` as a parameter and checks whether there is any overlap between the live ranges of the two sets. If any of the live ranges in the receiver live set intersect with any of the live ranges in the second live set, it will return true. Otherwise, it will return false.


Internal Representation and Key Implementation Points.

    Instance Variables
	definitions:		<Object>
	liveRanges:		<Object>
	location:		<Object>


    Implementation Points
"
Class {
	#name : 'DRLiveSet',
	#superclass : 'Object',
	#instVars : [
		'location',
		'liveRanges',
		'definitions'
	],
	#category : 'Druid-LinearScanRegisterAllocation',
	#package : 'Druid',
	#tag : 'LinearScanRegisterAllocation'
}

{ #category : 'accessing' }
DRLiveSet class >> definition: aDefinition [

	^ self new
		addDefinition: aDefinition;
		yourself
]

{ #category : 'testing' }
DRLiveSet >> addDefinition: aDefinition [

	definitions add: aDefinition
]

{ #category : 'accessing' }
DRLiveSet >> addDefinitions: aCollection [

	definitions addAll: aCollection
]

{ #category : 'ranges' }
DRLiveSet >> addLiveRange: aRange [
	
	"Add a liveness range that goes from the first instruction to just before the last instruction"
	liveRanges add: aRange
]

{ #category : 'accessing' }
DRLiveSet >> addLiveRanges: someLiveRanges [

	someLiveRanges do: [ :e | self addLiveRange: e ]
]

{ #category : 'ranges' }
DRLiveSet >> addRangeFrom: firstInstruction to: lastInstruction [

	"Add a liveness range that goes from the first instruction to just before the last instruction"

	self addLiveRange: (DRLiveRange new
			 start: firstInstruction;
			 stop: lastInstruction;
			 yourself).
]

{ #category : 'merging' }
DRLiveSet >> definitions [

	^ definitions
]

{ #category : 'testing' }
DRLiveSet >> initialize [
	super initialize.
	liveRanges := OrderedCollection new.
	definitions := Set new
]

{ #category : 'testing' }
DRLiveSet >> isActiveAt: position [

	^ liveRanges anySatisfy: [ :e | e isActiveAt: position ]
]

{ #category : 'accessing' }
DRLiveSet >> liveRanges [
	^ liveRanges
]

{ #category : 'accessing' }
DRLiveSet >> location [
	^ location
]

{ #category : 'accessing' }
DRLiveSet >> location: aLocation [
	
	location := aLocation
]

{ #category : 'merging' }
DRLiveSet >> mergeWith: aDRLiveSet [

	"Merge two non overlapping live sets"
	^ DRLiveSet new
		  addDefinitions: self definitions;
		  addDefinitions: aDRLiveSet definitions;
		  addLiveRanges: self liveRanges;
		  addLiveRanges: aDRLiveSet liveRanges;
		  yourself
]

{ #category : 'accessing' }
DRLiveSet >> numberOfUsers [
	
	^ definitions sum: [ :e | e users size ]
]

{ #category : 'testing' }
DRLiveSet >> overlaps: aDRLiveSet [ 
	
	^ liveRanges anySatisfy: [ :e | aDRLiveSet overlivesLiveRange: e ]
]

{ #category : 'testing' }
DRLiveSet >> overlivesLiveRange: aDRLiveRange [ 
	
	^ liveRanges anySatisfy: [ :e | e overlaps: aDRLiveRange ]
]

{ #category : 'printing' }
DRLiveSet >> printOn: aStream [

	super printOn: aStream.
	aStream << '['.
	aStream << self start asString.
	aStream << ', '.
	aStream << self stop asString.
	aStream << '['.
]

{ #category : 'printing' }
DRLiveSet >> size [

	^ self stop - self start
]

{ #category : 'accessing' }
DRLiveSet >> start [
	
	^ liveRanges min: [ :e | e start order ]
]

{ #category : 'accessing' }
DRLiveSet >> stop [
	
	^ liveRanges max: [ :e | e stop order ]
]
