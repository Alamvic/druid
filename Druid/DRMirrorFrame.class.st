Class {
	#name : #DRMirrorFrame,
	#superclass : #Object,
	#instVars : [
		'deferredMethodReturns',
		'returnValue',
		'token'
	],
	#category : #'Druid-Tests'
}

{ #category : #adding }
DRMirrorFrame >> addDeferredReturnState: aDRExecutionState [ 
	
	deferredMethodReturns at: aDRExecutionState second put: aDRExecutionState first
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
