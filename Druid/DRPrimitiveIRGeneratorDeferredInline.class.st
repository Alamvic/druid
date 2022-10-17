Class {
	#name : #DRPrimitiveIRGeneratorDeferredInline,
	#superclass : #DRPrimitiveIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRPrimitiveIRGeneratorDeferredInline >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	| newPrimitiveFailureCode method |
	method := self methodFor: aRBMessageNode withReceiver: receiver.

	"If method has deferred pragma, then resolve as message send (will be inlined during optimisation)"
	(method ast hasPragmaNamed: #druidDeferredInline) ifFalse: [ 
		^ super
			  resolveMessageSend: aRBMessageNode
			  receiver: receiver
			  arguments: arguments ].
	self
		addInstructionWithNoResultFrom: aRBMessageNode
		instructionKind: DRStore
		operands: { 
				8 asDRValue.
				executionState primitiveFailureCode.
				DRInterpreterReference primitiveFailureCodeAddress }.
	self popOperand.
	self
		messageSendInstructionFor: aRBMessageNode
		receiver: receiver
		arguments: arguments
		method: method.
	newPrimitiveFailureCode := self
		                           addInstructionFrom: aRBMessageNode
		                           instructionKind: DRLoad
		                           operands: { 
				                           8 asDRValue.
				                           DRInterpreterReference
					                           primitiveFailureCodeAddress }.
	self popOperand.
	executionState primitiveFailureCode: newPrimitiveFailureCode
]
