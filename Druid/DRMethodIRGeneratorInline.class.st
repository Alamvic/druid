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

{ #category : 'building' }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> visitTemporaryVariableNode: aRBVariableNode [

	| value |
	value := self temporaryAt: aRBVariableNode name.

	aRBVariableNode isTempVariable ifTrue: [
		| store |
		store := self topFrame temporaryAt: aRBVariableNode name withState: executionState.

		^ self
			  addInstructionFrom: aRBVariableNode
			  instructionKind: DRLoadTemporaryVariable
			  operands: {
					  aRBVariableNode name asDRValue.
					  store } ].

	"Else, it is an argument"
	^ self pushOperand: value
]
