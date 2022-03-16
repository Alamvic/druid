Class {
	#name : #DRSCCPEdge,
	#superclass : #DREdge,
	#instVars : [
		'executed'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRSCCPEdge >> executed: anObject [

	executed := anObject
]

{ #category : #accessing }
DRSCCPEdge >> initialize [

	super initialize.
	executed := false
]

{ #category : #accessing }
DRSCCPEdge >> isExecuted [

	^ executed
]

{ #category : #accessing }
DRSCCPEdge >> mark [ 

	executed := true
]
