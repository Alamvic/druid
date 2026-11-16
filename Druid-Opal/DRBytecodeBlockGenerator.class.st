Class {
	#name : 'DRBytecodeBlockGenerator',
	#superclass : 'DRBytecodeGenerator',
	#instVars : [
		'blockScope'
	],
	#category : 'Druid-Opal-BytecodeGeneration',
	#package : 'Druid-Opal',
	#tag : 'BytecodeGeneration'
}

{ #category : 'as yet unclassified' }
DRBytecodeBlockGenerator >> blockScope: aDRScope [

	blockScope := aDRScope
]

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
DRBytecodeBlockGenerator >> interpretInitializeScope: scope [

	| args  |	
	args := scope argumentNames.
	
	"Add arguments"
	args do: [ :arg| 
		builder addTemp: arg ].
	
	"copied vars + tempVector"
	scope outerScope outerCopiedVarNames, scope copiedVarNames do: [ : var | 
		builder addTemp: var ].
	
	"Add local temp"
	scope tempVarNames do: [ :var |
		builder addTemp: var ].
		
	"Add own temp vectors (could be many due to inlinings)"
	scope definedTempVectors keysAndValuesDo: [ :vector :temps |
		self createTempVector: temps vectorName: vector
		 ].
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> isResultInstruction: aDRInstruction [

	^ aDRInstruction users isEmpty or: [
			  aDRInstruction isStore and: [
				  aDRInstruction users anySatisfy: [ :i | i isReturn not ] ] ]
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitBlockReturn: aDBlockRReturn [ 

	aDBlockRReturn operand1 acceptVisitor: self.
	builder blockReturnTop.
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	blockScope wasMethodInlined ifTrue: [ 
		Error signal: 'Non-local return on inlined method breaks the semantics' ].

	super visitReturn: aDRReturn
]
