"
Abstract class to provide a source method, i.e., for example, in the interpreter the source method contains the method which the primitive implements.

"
Class {
	#name : #DRInterpreterInstruction,
	#superclass : #Object,
	#instVars : [
		'sourceMethod',
		'cfg'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #accessing }
DRInterpreterInstruction >> cfg [

	^ cfg
]

{ #category : #accessing }
DRInterpreterInstruction >> cfg: aDRPrimitiveControlFlowGraph [ 

	cfg := aDRPrimitiveControlFlowGraph
]

{ #category : #testing }
DRInterpreterInstruction >> hasPragmaNamed: aString [

	^ self sourceMethod hasPragmaNamed: aString
]

{ #category : #accessing }
DRInterpreterInstruction >> sourceMethod [

	^ sourceMethod
]

{ #category : #accessing }
DRInterpreterInstruction >> sourceMethod: anObject [

	sourceMethod := anObject
]

{ #category : #accessing }
DRInterpreterInstruction >> sourceSelector [

	^ self sourceMethod selector
]
