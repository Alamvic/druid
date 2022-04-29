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
