Class {
	#name : #DRIRTest,
	#superclass : #TestCase,
	#instVars : [
		'compilerCompiler'
	],
	#category : #'Druid-Tests'
}

{ #category : #edges }
DRIRTest >> anyIncomingEdge: cfg for: aBlock [

	^ (cfg incomingEdgesFor: aBlock) first "unique?"
]

{ #category : #edges }
DRIRTest >> anyOutgoingEdge: cfg for: aBlock [

	^ (cfg outgoingEdgesFor: aBlock) first "unique?"
]

{ #category : #assertions }
DRIRTest >> assertBlock: aBlock hasSameInstructions: expectedInstructions [

	self
		assertCollection: aBlock instructions allButLast
		hasSameElements: expectedInstructions.

]

{ #category : #assertions }
DRIRTest >> assertBlock: aBlock jumpsTo: nextBlock [

	self assert: (aBlock successors includes: nextBlock).

]

{ #category : #assertions }
DRIRTest >> assertConstraint: aConstraint includes: includedConstraint [

	self assert: (aConstraint includes: includedConstraint)
]

{ #category : #running }
DRIRTest >> configureInterpreter [

	compilerCompiler interpreter: DruidTestInterpreter basicNew 
]

{ #category : #assertions }
DRIRTest >> denyConstraint: aConstraint includes: includedConstraint [

	self deny: (aConstraint includes: includedConstraint)
]

{ #category : #test }
DRIRTest >> generateBasicDruidIRFor: methodSelector [

	| method cfg |
	method := DruidTestInterpreter lookupSelector: methodSelector.
	cfg := compilerCompiler generateDruidIRFor: method.
	^ cfg
]

{ #category : #test }
DRIRTest >> generateDruidIRFor: methodSelector [

	| cfg |
	cfg := self generateBasicDruidIRFor: methodSelector.
	self optimize: cfg.
	^ cfg
]

{ #category : #test }
DRIRTest >> newConditionalJump [

	^ compilerCompiler instructionFactory jumpIf: false
]

{ #category : #test }
DRIRTest >> optimize: cfg [

	compilerCompiler optimize: cfg
]

{ #category : #running }
DRIRTest >> setUp [

	super setUp.

	compilerCompiler := DRPrimitiveCompilerCompiler new
]

{ #category : #test }
DRIRTest >> setUpCFG: n [
	"Creates a sequence of n + 1 blocks: |0| -> |1| -> ... -> |n|"

	| cfg b0 b1 b2 b3 |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg initialBasicBlock.
	cfg b0: b0.
	n = 0 ifTrue: [ 
		b0 jumpTo: cfg newBasicBlock.
		^ cfg ].
	
	b1 := cfg newBasicBlock.
	cfg b1: b1.
	b0 jumpTo: b1.
	n = 1 ifTrue: [ 
		b1 jumpTo: cfg newBasicBlock.
		^ cfg ].
	
	b2 := cfg newBasicBlock.
	cfg b2: b2.
	b1 jumpTo: b2.
	n = 2 ifTrue: [ 
		b2 jumpTo: cfg newBasicBlock.
		^ cfg ].

	b3 := cfg newBasicBlock.
	cfg b3: b3.
	b2 jumpTo: b3.
	n = 3 ifTrue: [ 
		b3 jumpTo: cfg newBasicBlock.
		^ cfg ].

	self error: 'Unsupported graph size: ', n asString
]

{ #category : #test }
DRIRTest >> setUpCFGWithConditional [

	^ self setUpCFGWithConditional: self newConditionalJump.

]

{ #category : #test }
DRIRTest >> setUpCFGWithConditional: jump [
	"|0| -> |1| -> |2| -> |4|
             \ -> |3| --^
	"

	| cfg b0 b1 b2 b3 b4|
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock. 
	
	b4 endInstruction: DRNoop new. 
	b0 jumpTo: b1. 
	jump operand1 isInstruction ifTrue: [
		b1 addInstruction: jump operand1. 
	].
	b1 endInstruction: jump. 
	jump trueBranch: b2.
	b2 addPredecessor: b1. 
	jump falseBranch: b3.
	b3 addPredecessor: b1. 
	b2 jumpTo: b4. 
	b3 jumpTo: b4.
	
	cfg b0: b0.
	cfg b1: b1. 
	cfg b2: b2. 
	cfg b3: b3. 
	cfg b4: b4. 

	^ cfg. 



















]

{ #category : #test }
DRIRTest >> setUpCFGWithConditionalWithPhi [

	| cfg copy1 copy2 |
	cfg := self setUpCFGWithConditional.
	copy1 := cfg b2 copy: 1.
	copy2 := cfg b3 copy: 2.
	cfg phi: (cfg b4 phiWith: copy1 with: copy2).
	^ cfg
]

{ #category : #initialization }
DRIRTest >> setUpCFGWithLoop [

	| cfg preHeader initialValue loopPhi loopInstruction header loopBody loopExit |
	cfg := self setUpCFGWithConditionalWithPhi.
	cfg b4 phiFunctions unique removeFromCFG.

	header := cfg b1.
	preHeader := header newEmptyPredecessor.
	loopBody := cfg b2.	
	loopExit := cfg b3.
	

	initialValue := preHeader copy: 0.

	loopBody backJumpTo: cfg b1.
	loopInstruction := loopBody firstInstruction.
	loopPhi := header phiWith: loopInstruction with: initialValue.

	loopInstruction replaceBy: (loopBody copy: loopPhi).
	loopExit firstInstruction replaceBy: (loopExit copy: loopPhi).

	^ cfg
]
