Class {
	#name : 'DRMethodIRGeneratorInline',
	#superclass : 'DRMethodIRGenerator',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> currentBasicBlock: aDRBasicBlock [

	currentBasicBlock := aDRBasicBlock
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	| lastBlock |
	lastBlock := currentBasicBlock.
	"Non-local returns can add new returns to any frame, pop all frames to resolve them"
	[ executionState executionStack size = 0 ] whileFalse: [ self popFrame ].
	currentBasicBlock := lastBlock
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> handleCFGScope: methodNode [

	"here to handle the scope with inlining"
]

{ #category : 'testing' }
DRMethodIRGeneratorInline >> isBaseMethodArgument: aRBVariableNode [

	"Here we check if the variable is accessing to a temporary (mainly arguments) from the compiled method.
	This change between accessing a variable from the method to inline and from the method where it is inlined."

	^ executionState baseFrame method = aRBVariableNode variable scope node
]

{ #category : 'building' }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> visitTemporaryVariableNode: aRBVariableNode [

	aRBVariableNode isTempVariable ifTrue: [
		| store |
		store := self topFrame
			         temporaryAt: aRBVariableNode name
			         withState: executionState.

		^ self
			  addInstructionFrom: aRBVariableNode
			  instructionKind: DRLoadTemporaryVariable
			  operands: {
					  aRBVariableNode name asDRValue.
					  store } ].

	"Else, it is an argument"
	(self isBaseMethodArgument: aRBVariableNode)
		ifTrue: [
			| loadArg |
			loadArg := self
				           addInstructionFrom: aRBVariableNode
				           instructionKind: DRLoadArgument
				           operands: { aRBVariableNode variable index asDRValue }.
			loadArg argName: aRBVariableNode name.
			^ loadArg ]
		ifFalse: [
			| value |
			"If it is not accesssing a method argument, 
			then get the target instruction and push it"
			value := self temporaryAt: aRBVariableNode name.
			^ self pushOperand: value ]
]
