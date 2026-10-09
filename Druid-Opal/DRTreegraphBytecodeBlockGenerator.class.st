Class {
	#name : 'DRTreegraphBytecodeBlockGenerator',
	#superclass : 'DRTreegraphBytecodeGenerator',
	#instVars : [
		'blockScope'
	],
	#category : 'Druid-Opal-BytecodeGeneration',
	#package : 'Druid-Opal',
	#tag : 'BytecodeGeneration'
}

{ #category : 'accessing' }
DRTreegraphBytecodeBlockGenerator >> blockScope: aDRScope [

	blockScope := aDRScope
]

{ #category : 'compiling' }
DRTreegraphBytecodeBlockGenerator >> compileBlockFrom: aDRControlFlowGraph withScope: aScope [ 
	
	| ir compiledBlock |
	opalBytecodeBuilder numArgs: aDRControlFlowGraph numArgs.
	
	ir := opalBytecodeBuilder ir.
	
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 
]

{ #category : 'code-generation' }
DRTreegraphBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [
	
	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBasicBlock: block ].
	

]

{ #category : 'visiting' }
DRTreegraphBytecodeBlockGenerator >> visitBlockReturn: aDBlockRReturn [

	opalBytecodeBuilder blockReturnTop 
	
]

{ #category : 'ir-to-target' }
DRTreegraphBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	blockScope wasMethodInlined ifTrue: [ 
		Error signal: 'Non-local return on inlined method breaks the semantics' ].

	super visitReturn: aDRReturn
]
