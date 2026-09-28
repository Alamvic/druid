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
	builder numArgs: aDRControlFlowGraph numArgs.
	
	ir := builder ir.
	
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 
]

{ #category : 'code-generation' }
DRNewBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [
	
	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBasicBlock: block ].
	

]

{ #category : 'testing' }
DRNewBytecodeBlockGenerator >> isResultInstruction: aDRInstruction [

	^ aDRInstruction users isEmpty or: [
			  aDRInstruction isStore and: [
				  aDRInstruction users anySatisfy: [ :i | i isReturn not ] ] ]
]

{ #category : 'visiting' }
DRNewBytecodeBlockGenerator >> visitBlockReturn: aDBlockRReturn [ 

	aDBlockRReturn operand1 isInstruction ifFalse: [ aDBlockRReturn operand1 acceptVisitor: self ].
	builder blockReturnTop.
]

{ #category : 'ir-to-target' }
DRNewBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	blockScope wasMethodInlined ifTrue: [ 
		Error signal: 'Non-local return on inlined method breaks the semantics' ].

	super visitReturn: aDRReturn
]
