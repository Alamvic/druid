Class {
	#name : #DROptimisationTest,
	#superclass : #TestCase,
	#instVars : [
		'compilerCompiler'
	],
	#category : #'Druid-Tests'
}

{ #category : #testing }
DROptimisationTest class >> isAbstract [

	^ self class == DROptimisationTest
]

{ #category : #running }
DROptimisationTest >> configureInterpreter [

	super setUp.

	compilerCompiler interpreter: DruidTestInterpreter new
]

{ #category : #'as yet unclassified' }
DROptimisationTest >> generateDruidIRFor: methodSelector [

	| method cfg |
	method := DruidTestInterpreter lookupSelector: methodSelector.
	cfg := compilerCompiler generateDruidIRFor: method.
	(DRPhiSimplication then: DRDeadCodeElimination) applyTo: cfg.

	^ cfg
]

{ #category : #'as yet unclassified' }
DROptimisationTest >> generatePaths: cfg [ 
	compilerCompiler generatePaths: cfg
]

{ #category : #'as yet unclassified' }
DROptimisationTest >> optimize: cfg [

	compilerCompiler optimize: cfg
]

{ #category : #running }
DROptimisationTest >> setUp [

	super setUp.

	compilerCompiler := DRPrimitiveCompilerCompiler new
]

{ #category : #test }
DROptimisationTest >> setUpCFG: n [
	"Creates a sequence of n + 1 blocks: |0| -> |1| -> ... -> |n|"

	| cfg b0 b1 b2 b3 |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg initialBasicBlock.
	cfg b0: b0.
	n = 0 ifTrue: [ ^ cfg ].
	
	b1 := cfg newBasicBlock.
	cfg b1: b1.
	b0 jumpTo: b1.
	n = 1 ifTrue: [ ^ cfg ].
	
	b2 := cfg newBasicBlock.
	cfg b2: b2.
	b1 jumpTo: b2.
	n = 2 ifTrue: [ ^ cfg ].

	b3 := cfg newBasicBlock.
	cfg b3: b3.
	b2 jumpTo: b3.
	n = 3 ifTrue: [ ^ cfg ].

	self error: 'Unsupported graph size: ', n asString
]

{ #category : #test }
DROptimisationTest >> setUpCFGWithConditional: jump [
	"|init| -> |0| -> |1| -> |2| -> |4|
							  \ -> |3| --^
	"

	| cfg b0 b1 b2 b3 b4|
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock. 
	
	cfg initialBasicBlock jumpTo: b0.
	
	b4 endInstruction: DRNoop new. 
	b0 jumpTo: b1. 
	jump condition isInstruction ifTrue: [
		b1 addInstruction: jump condition. 
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
