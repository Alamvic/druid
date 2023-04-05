"
Abstract class to provide a source method, i.e., for example, in the interpreter the source method contains the method which the primitive implements.

"
Class {
	#name : #DRDispatchTableObject,
	#superclass : #Object,
	#instVars : [
		'sourceMethod',
		'cfg'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #accessing }
DRDispatchTableObject >> cfg [

	^ cfg
]

{ #category : #accessing }
DRDispatchTableObject >> cfg: aDRPrimitiveControlFlowGraph [ 

	cfg := aDRPrimitiveControlFlowGraph
]

{ #category : #testing }
DRDispatchTableObject >> hasPragmaNamed: aString [

	^ self sourceMethod hasPragmaNamed: aString
]

{ #category : #accessing }
DRDispatchTableObject >> sourceMethod [

	^ sourceMethod
]

{ #category : #accessing }
DRDispatchTableObject >> sourceMethod: anObject [

	sourceMethod := anObject
]

{ #category : #accessing }
DRDispatchTableObject >> sourceSelector [

	^ self sourceMethod selector
]
