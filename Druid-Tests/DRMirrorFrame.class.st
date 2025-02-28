Class {
	#name : 'DRMirrorFrame',
	#superclass : 'Object',
	#instVars : [
		'deferredMethodReturns',
		'poppedValue'
	],
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'adding' }
DRMirrorFrame >> addDeferredReturnState: aDRExecutionState [ 
	
	deferredMethodReturns at: aDRExecutionState second put: aDRExecutionState first
]

{ #category : 'deferred returns' }
DRMirrorFrame >> clearDeferredMethodReturns [

	deferredMethodReturns removeAll
]

{ #category : 'accessing' }
DRMirrorFrame >> deferredMethodReturns [

	^ deferredMethodReturns
]

{ #category : 'initialization' }
DRMirrorFrame >> initialize [
	super initialize.
	deferredMethodReturns := Dictionary new
]

{ #category : 'accessing' }
DRMirrorFrame >> poppedValue [

	^ poppedValue
]

{ #category : 'accessing' }
DRMirrorFrame >> poppedValue: aDRValue [

	^ poppedValue := aDRValue
]
