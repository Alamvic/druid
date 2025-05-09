Class {
	#name : 'DRProfileBytecodeGenerator',
	#superclass : 'DRBytecodeGenerator',
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'adding' }
DRProfileBytecodeGenerator >> addProfileFor: aDRMessageSend [

	builder pushLiteralVariable: DRProfileInstrumentation binding.
	aDRMessageSend receiver acceptVisitor: self.
	builder send: #sendingMessageTo:.
	builder popTop
]

{ #category : 'visiting' }
DRProfileBytecodeGenerator >> visitMessageSend: aDRMessageSend [

	(DRProfileInstrumentation class selectors includes:
		 aDRMessageSend selector) ifFalse: [
		self addProfileFor: aDRMessageSend ].
	super visitMessageSend: aDRMessageSend
]
