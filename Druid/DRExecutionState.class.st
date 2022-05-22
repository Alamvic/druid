Class {
	#name : #DRExecutionState,
	#superclass : #Object,
	#instVars : [
		'vmState',
		'frameState'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRExecutionState >> frameState [

	^ frameState
]

{ #category : #accessing }
DRExecutionState >> frameState: anObject [

	frameState := anObject
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
