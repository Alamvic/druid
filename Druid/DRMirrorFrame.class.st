Class {
	#name : #DRMirrorFrame,
	#superclass : #Object,
	#instVars : [
		'deferredMethodReturns',
		'returnValue'
	],
	#category : #'Druid-Tests'
}

{ #category : #'deferred returns' }
DRMirrorFrame >> clearDeferredMethodReturns [

	deferredMethodReturns copy keysDo: [ :key | deferredMethodReturns removeKey: key ]
]

{ #category : #accessing }
DRMirrorFrame >> deferredMethodReturns [

	^ deferredMethodReturns
]

{ #category : #accessing }
DRMirrorFrame >> deferredMethodReturns: anObject [

	deferredMethodReturns := anObject
]

{ #category : #accessing }
DRMirrorFrame >> returnValue [

	^ returnValue
]

{ #category : #accessing }
DRMirrorFrame >> returnValue: aDRValue [

	self flag: #TODO. "Rename"

	^ returnValue := aDRValue
]
