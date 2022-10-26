Class {
	#name : #DRMethodIRGeneratorInline,
	#superclass : #DRMethodIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRMethodIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	| lastBlock |
	lastBlock := currentBasicBlock.
	"Pop until end for deferred returns"
	[ executionState executionStack size = 0 ] whileFalse: [ self popFrame ].
	currentBasicBlock := lastBlock
]

{ #category : #building }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]
