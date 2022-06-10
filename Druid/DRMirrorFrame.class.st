Class {
	#name : #DRMirrorFrame,
	#superclass : #Object,
	#instVars : [
		'deferredMethodReturns'
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

{ #category : #initialization }
DRMirrorFrame >> initialize [
	super initialize.
	deferredMethodReturns := Dictionary new
]
