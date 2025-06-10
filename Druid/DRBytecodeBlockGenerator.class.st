Class {
	#name : 'DRBytecodeBlockGenerator',
	#superclass : 'DRBytecodeGenerator',
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'compiling' }
DRBytecodeBlockGenerator >> compileBlockFrom: aDRControlFlowGraph withScope: aScope [ 
	
	| ir compiledBlock irTranslator|
	builder numArgs: aDRControlFlowGraph numArgs.
	
	ir := builder ir.
	
	irTranslator := DRIRToBytecodeTranslator new compilationContext: ir compilationContext; yourself. 
	compiledBlock := irTranslator compileBlockIR: ir withScope: aScope.
	
	^ compiledBlock 
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [
	
	self handleTemporaryVariableFromCFG: aDRControlFlowGraph.
	
	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ].
	

]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> isResultInstruction: aDRInstruction [

	^ aDRInstruction users isEmpty or: ([ aDRInstruction isStore and: [aDRInstruction users anySatisfy: [:i | i isReturn not]]])
	

]
