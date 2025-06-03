Class {
	#name : 'DRBytecodeBlockGenerator',
	#superclass : 'DRBytecodeGenerator',
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'adding' }
DRBytecodeBlockGenerator >> addOuterVector: vectorName withVars: variableNames [ 
	self shouldBeImplemented.
]

{ #category : 'compiling' }
DRBytecodeBlockGenerator >> compileBlockFrom: aDRControlFlowGraph withScope: aScope [ 
	
	| ir compiledBlock|
	builder numArgs: aDRControlFlowGraph numArgs.
	
	ir := builder ir.
	
	compiledBlock := ir compiledBlock: aScope.
	
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

{ #category : 'accessing' }
DRBytecodeBlockGenerator >> vectorTempName [

	"hardcoded for now, name comes from the default scope, and we use it for now"
	^ '1vector1' ", self vectorTempIndex asString"
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> vectorTempNameFromTempVar: aTemporaryVariable [

	^ aTemporaryVariable isTempVectorTemp
		  ifTrue: [ super vectorTempName ]
		  ifFalse: [ self vectorTempName ]
]
