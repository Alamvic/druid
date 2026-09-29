Class {
	#name : 'DRNewBytecodeBlockGenerator',
	#superclass : 'DRNewBytecodeGenerator',
	#instVars : [
		'blockScope'
	],
	#category : 'Druid-Opal-BytecodeGeneration',
	#package : 'Druid-Opal',
	#tag : 'BytecodeGeneration'
}

{ #category : 'accessing' }
DRNewBytecodeBlockGenerator >> blockScope: aDRScope [

	blockScope := aDRScope
]

{ #category : 'compiling' }
DRNewBytecodeBlockGenerator >> compileBlockFrom: aDRControlFlowGraph withScope: aScope [ 
	
	| ir compiledBlock |
	opalBytecodeBuilder numArgs: aDRControlFlowGraph numArgs.
	
	ir := opalBytecodeBuilder ir.
	
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 
]

{ #category : 'code-generation' }
DRNewBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [
	
	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBasicBlock: block ].
	

]

{ #category : 'visiting' }
DRNewBytecodeBlockGenerator >> visitBlockReturn: aDBlockRReturn [

	opalBytecodeBuilder blockReturnTop 
	
]

{ #category : 'ir-to-target' }
DRNewBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	blockScope wasMethodInlined ifTrue: [ 
		Error signal: 'Non-local return on inlined method breaks the semantics' ].

	super visitReturn: aDRReturn
]
