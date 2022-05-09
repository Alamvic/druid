Class {
	#name : #DRLiveRange,
	#superclass : #Object,
	#instVars : [
		'start',
		'stop'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #testing }
DRLiveRange >> isActiveAt: aPosition [
	
	^ aPosition between: self start order and: self stop order
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
	aStream << '('.
	aStream << self orderInterval asString.
	aStream << ')'.
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
