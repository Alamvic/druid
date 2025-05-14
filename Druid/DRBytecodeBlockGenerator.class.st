Class {
	#name : 'DRBytecodeBlockGenerator',
	#superclass : 'DRBytecodeGenerator',
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> compileBlockWithScope: aScope [

	| ir compiledBlock|
	
	ir := builder ir.
	compiledBlock := ir compiledBlock: aScope.
	
	^ compiledBlock 

]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> generateTargetASTFromIR: aDRControlFlowGraph [

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ]
]

{ #category : 'ir-to-target' }
DRBytecodeBlockGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder blockReturnTop.
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> visitStoreTempVar: aDRStoreTemporaryVariable [

	| value |
	value := aDRStoreTemporaryVariable operand2 simpleConstantFold asDRValue.
	value isStoreTemporaryVariable
		ifTrue: [ builder pushTemp: value operand1 value ]
		ifFalse: [ value acceptVisitor: self ].

	builder addTemp: aDRStoreTemporaryVariable operand1 value.
	builder storeTemp: aDRStoreTemporaryVariable operand1 value.
	builder popTop
]

{ #category : 'code-generation' }
DRBytecodeBlockGenerator >> visitTemporaryVariable: aDRLoadTemporaryVariable [

	| tempName |
	tempName := aDRLoadTemporaryVariable operand1 value.
	
	builder addTemp: tempName.
	builder pushTemp: tempName
]
