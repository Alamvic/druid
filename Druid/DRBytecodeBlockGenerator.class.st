Class {
	#name : 'DRBytecodeBlockGenerator',
	#superclass : 'DRBytecodeGenerator',
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> compileBlockWithScope: aScope [

	| ir compiledBlock|
	
	ir := builder ir.
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 

]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [

	self handleTemporaryVariable: aDRControlFlowGraph.

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ]
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder blockReturnTop.
]
