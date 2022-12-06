Class {
	#name : #DRMethodIRGeneratorInline,
	#superclass : #DRMethodIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRMethodIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	| lastBlock |
	lastBlock := currentBasicBlock.
	"Non-local returns can add new returns to any frame, pop all frames to resolve them"
	[ executionState executionStack size = 0 ] whileFalse: [ self popFrame ].
	currentBasicBlock := lastBlock
]

{ #category : #building }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]
