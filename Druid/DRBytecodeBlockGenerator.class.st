Class {
	#name : 'DRBytecodeBlockGenerator',
	#superclass : 'DRBytecodeGenerator',
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'compiling' }
DRBytecodeBlockGenerator >> compileBlockFrom: aDRControlFlowGraph withScope: aScope [ 
	
	| ir compiledBlock|
	
	ir := builder ir.
	1 halt.
	"add vectorTempVar to the scope of the block"
	"aDRControlFlowGraph allVectorTemporaryInstructions do: [ :var |
		aScope addTemp: var.
		 ]."
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> compileBlockWithScope: aScope [

	| ir compiledBlock|
	
	ir := builder ir.
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 

]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [

	| args temps vectorTemps temporariesList |
	"Construct the tempVector"
	args := aDRControlFlowGraph argumentNames.
	temps := aDRControlFlowGraph temporaryVariableNames. 
	vectorTemps := aDRControlFlowGraph vectorTemporaryVariableNames .
	1 halt.
	
	args do: [ :arg| 
		builder addTemp: arg ].
	1 halt.
	"vectorTemps do: [ :var| 
		builder addTemp: var ]."
	
	builder addTemp: self vectorTempName.
	
	temporariesList := args , temps.
	temporariesList isNotEmpty ifTrue: [
		builder createTempVectorNamed: 'vector1' withVars: temporariesList.].
	
	args do: [ :arg | 
		builder pushTemp: arg.
		builder storeRemoteTemp: arg inVector: self vectorTempName.
		builder popTop ].

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ]
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder blockReturnTop.
]
