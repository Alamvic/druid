Class {
	#name : #DRLiveRange,
	#superclass : #Object,
	#instVars : [
		'start',
		'stop'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRLiveRange >> orderInterval [

	^ self start order to: self stop order - 1
]

{ #category : #testing }
DRLiveRange >> overlaps: aDRLiveRange [ 

	^ (self orderInterval intersection: aDRLiveRange orderInterval) notEmpty
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
