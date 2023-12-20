Class {
	#name : #DRCogitStackToRegisterGeneratorTest,
	#superclass : #DRCogitGeneratorTest,
	#category : #'Druid-Tests'
}

{ #category : #helpers }
DRCogitStackToRegisterGeneratorTest >> setUp [

	super setUp.
	generator := DRCogitStackToRegisterMappingGenerator new
]

{ #category : #tests }
DRCogitStackToRegisterGeneratorTest >> testGenerateIf [

	| cfg |
	cfg := self setUpCFGWithConditional.
	DRStager new applyTo: cfg.
	generator generateCodeForCFG: cfg.

	self assert: self statements second selector equals: #ifTrue:
]

{ #category : #tests }
DRCogitStackToRegisterGeneratorTest >> testGenerateMoveConstant [

	| cfg r1 generatedASTInstruction |
	cfg := DRControlFlowGraph new.
	r1 := cfg instructionFactory copy: DRInterpreterReference nilObject.

	self generateCogitForInstruction: r1.
	generatedASTInstruction := self statements last.

	self
		assert: generatedASTInstruction selector
		equals: #genMoveConstant:R:.
	self
		assert: generatedASTInstruction arguments first name
		equals: 'objectMemory nilObject'
]

{ #category : #tests }
DRCogitStackToRegisterGeneratorTest >> testGeneratePushBaseOffset [

	| cfg basicBlock r1 orASTInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		              r1 := block
			                    pushBase: block loadFramePointer
			                    offset: 17 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	self generateCogitForInstruction: r1.
	orASTInstruction := self statements detect: [ :e | e isMessage ].

	self assert: orASTInstruction selector equals: #ssPushBase:offset:.
	self assert: orASTInstruction arguments first name equals: 't0'.
	self assert: orASTInstruction arguments second value equals: 17
]

{ #category : #tests }
DRCogitStackToRegisterGeneratorTest >> testGenerateWhile [

	| cfg |
	cfg := self setUpCFGWithLoop.
	DRStager new applyTo: cfg.
	generator generateCodeForCFG: cfg.

	self assert: self statements fourth selector equals: #whileTrue:
]
