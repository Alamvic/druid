Class {
	#name : 'DRBytecodeBlockGenerator',
	#superclass : 'DRBytecodeGenerator',
	#instVars : [
		'sendScope'
	],
	#category : 'Druid-Opal-BytecodeGeneration',
	#package : 'Druid-Opal',
	#tag : 'BytecodeGeneration'
}

{ #category : 'compiling' }
DRBytecodeBlockGenerator >> compileBlockFrom: aDRControlFlowGraph withScope: aScope [ 
	
	| ir compiledBlock |
	builder numArgs: aDRControlFlowGraph numArgs.
	
	ir := builder ir.
	
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [
	
	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ].
	

]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> isResultInstruction: aDRInstruction [

	^ aDRInstruction users isEmpty or: [
			  aDRInstruction isStore and: [
				  aDRInstruction users anySatisfy: [ :i | i isReturn not ] ] ]
]

{ #category : 'as yet unclassified' }
DRBytecodeBlockGenerator >> sendScope: aDRScope [

	sendScope := aDRScope
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitBlockReturn: aDBlockRReturn [ 

	aDBlockRReturn operand1 acceptVisitor: self.
	builder blockReturnTop.
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	sendScope isInlined ifTrue: [ 
		Error signal: 'Non-local return on inlined method breaks the semantics' ].

	super visitReturn: aDRReturn
]
