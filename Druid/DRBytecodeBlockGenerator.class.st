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

	| args temps temporariesList outerTemps |
	
	args := aDRControlFlowGraph argumentNames.
	outerTemps := aDRControlFlowGraph vectorTemporaryVariableNames.
	temps := aDRControlFlowGraph temporaryVariableNames difference: outerTemps. 
	
	args do: [ :arg| 
		builder addTemp: arg ].
	
	outerTemps isNotEmpty ifTrue:[
		builder addTemp: super vectorTempName].
	
	
	temporariesList := args , temps.
	temporariesList isNotEmpty ifTrue: [
		builder createTempVectorNamed: self vectorTempName withVars: temporariesList.].
	
	args do: [ :arg | 
		builder pushTemp: arg.
		builder storeRemoteTemp: arg inVector: self vectorTempName.
		builder popTop ].

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ].
	

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
