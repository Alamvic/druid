Class {
	#name : #DRMirrorFrame,
	#superclass : #Object,
	#instVars : [
		'deferredMethodReturns',
		'returnValue'
	],
	#category : #'Druid-Tests'
}

{ #category : #adding }
DRMirrorFrame >> addDeferredReturnState: aDRExecutionState [ 
	
	deferredMethodReturns at: aDRExecutionState second put: aDRExecutionState first
]

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

{ #category : #initialization }
DRMirrorFrame >> initialize [
	super initialize.
	deferredMethodReturns := Dictionary new
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
