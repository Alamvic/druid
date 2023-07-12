Class {
	#name : #DRProfileInstrumentation,
	#superclass : #Object,
	#classInstVars : [
		'default'
	],
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #'not instrument' }
DRProfileInstrumentation class >> class [
	"Hack to not instrument this messages"

	^ super class
]

{ #category : #accessing }
DRProfileInstrumentation class >> default [

	^ default ifNil: [ default := self new ]
]

{ #category : #accessing }
DRProfileInstrumentation class >> default: anObject [

	default := anObject
]

{ #category : #'not instrument' }
DRProfileInstrumentation class >> instVarAt: index [
	"Hack to not instrument this messages"

	^ super instVarAt: index
]

{ #category : #listener }
DRProfileInstrumentation class >> sendingMessageTo: anObject [

	self default sendingMessageTo: anObject
]

{ #category : #listener }
DRProfileInstrumentation >> sendingMessageTo: anObject [

	| callerContext receiverNode |
	callerContext := thisContext sender findContextSuchThat: [ :context | 
		"Find the last method executed before the instrumentation"
		                 context receiver ~= self class and: [
			                 context receiver class ~= self class ] ].
	receiverNode := callerContext method sourceNodeForPC:
		                callerContext pc + 1.

	DRProfileBasedTypeSystem default addReceiverInfo: anObject forNode: receiverNode
]
