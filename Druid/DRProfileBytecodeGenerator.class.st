Class {
	#name : #DRProfileBytecodeGenerator,
	#superclass : #DRBytecodeGenerator,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #adding }
DRProfileBytecodeGenerator >> addProfileFor: aDRMessageSend [

	builder pushLiteralVariable: DRProfileBasedTypeSystem binding.
	aDRMessageSend receiver acceptVisitor: self.
	builder send: #sendingMessageTo:.
	builder popTop
]

{ #category : #visiting }
DRProfileBytecodeGenerator >> visitMessageSend: aDRMessageSend [
	
	self addProfileFor: aDRMessageSend.
	super visitMessageSend: aDRMessageSend
]
