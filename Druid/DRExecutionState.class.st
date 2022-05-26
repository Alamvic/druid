Class {
	#name : #DRExecutionState,
	#superclass : #Object,
	#instVars : [
		'vmState',
		'frameState'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accumulating }
DRExecutionState >> addTo: aDRExecutionState [ 
	
	"Add my own state to the execution state in parameter"
	vmState addTo: aDRExecutionState vmState.
	frameState addTo: aDRExecutionState frameState
]

{ #category : #accessing }
DRExecutionState >> frameState [

	^ frameState
]

{ #category : #accessing }
DRExecutionState >> frameState: anObject [

	frameState := anObject
]

{ #category : #testing }
DRExecutionState >> isNotSimplified [
	
	^ vmState isNotSimplified and: [ frameState isNotSimplified ]
]

{ #category : #copying }
DRExecutionState >> postCopy [
	super postCopy.
	vmState := vmState copy.
	frameState := frameState copy.
]

{ #category : #accessing }
DRExecutionState >> vmState [

	^ vmState
]

{ #category : #accessing }
DRExecutionState >> vmState: anObject [

	vmState := anObject
]
