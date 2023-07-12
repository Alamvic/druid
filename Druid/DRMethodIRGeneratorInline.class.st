Class {
	#name : #DRMethodIRGeneratorInline,
	#superclass : #DRMethodIRGenerator,
	#instVars : [
		'typeSystem'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethodIRGeneratorInline >> currentBasicBlock: aDRBasicBlock [

	currentBasicBlock := aDRBasicBlock
]

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

{ #category : #accessing }
DRMethodIRGeneratorInline >> typeSystem [

	^ typeSystem ifNil: [ super typeSystem ]
]

{ #category : #accessing }
DRMethodIRGeneratorInline >> typeSystem: aDRTypeSystem [

	typeSystem := aDRTypeSystem
]
