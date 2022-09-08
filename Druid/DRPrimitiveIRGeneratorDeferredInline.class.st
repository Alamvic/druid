Class {
	#name : #DRPrimitiveIRGeneratorDeferredInline,
	#superclass : #DRPrimitiveIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRPrimitiveIRGeneratorDeferredInline >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments method: method [

	| instruction newPrimitiveFailureCode |
	(method ast hasPragmaNamed: #druidNotInline) ifFalse: [ 
		^ super
			  resolveMessageSend: aRBMessageNode
			  receiver: receiver
			  arguments: arguments
			  method: method ].

	"If method has deferred pragma, then resolve as message send (will be inlined during optimisation)"

	self
		addInstructionWithNoResultFrom: aRBMessageNode
		instructionKind: DRStore
		operands: { 
				8 asDRValue.
				executionState primitiveFailureCode.
				DRInterpreterReference primitiveFailureCodeAddress }.
	self popOperand.

	instruction := self
		               instantiate: DRMessageSend
		               operands: { receiver } , arguments.
	instruction methodNode: method ast.
	self addInstruction: instruction from: aRBMessageNode.

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
