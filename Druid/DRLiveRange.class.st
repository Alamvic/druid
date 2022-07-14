Class {
	#name : #DRLiveRange,
	#superclass : #Object,
	#instVars : [
		'start',
		'stop'
	],
	#category : #'Druid-LinearScanRegisterAllocation'
}

{ #category : #testing }
DRLiveRange >> isActiveAt: aPosition [
	
	"We consider that the stop of the live range is open at the end.
	"
	^ aPosition between: self start order and: self stop order - 1
]

{ #category : #accessing }
DRLiveRange >> orderInterval [

	^ self start order to: self stop order - 1
]

{ #category : #testing }
DRLiveRange >> overlaps: aDRLiveRange [ 

	^ (self orderInterval intersection: aDRLiveRange orderInterval) notEmpty
]

{ #category : #printing }
DRLiveRange >> printOn: aStream [

	super printOn: aStream.
	aStream << '['.
	aStream << self start order asString.
	aStream << ', '.
	aStream << self stop order asString.
	aStream << '['.
]

{ #category : #accessing }
DRLiveRange >> start [

	^ start
]

{ #category : #accessing }
DRLiveRange >> start: anObject [

	start := anObject
]

{ #category : #accessing }
DRLiveRange >> stop [

	^ stop
]

{ #category : #accessing }
DRLiveRange >> stop: anObject [

	stop := anObject
]
