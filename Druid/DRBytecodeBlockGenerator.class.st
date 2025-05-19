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
	
	ir := builder ir.
	
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [

	| args temps temporariesList |
	
	args := aDRControlFlowGraph argumentNames.
	temps := aDRControlFlowGraph temporaryVariableNames. 
	
	args do: [ :arg| 
		builder addTemp: arg ].
	
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

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> vectorTempNameFromTempVar: aTemporaryVariable [

	^ aTemporaryVariable isTempVectorTemp
		  ifTrue: [ self vectorTempName ]
		  ifFalse: [ 'vector1' ]
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder blockReturnTop.
]
