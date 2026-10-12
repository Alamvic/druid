Class {
	#name : 'DRFrameInfoCleanerTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'tests' }
DRFrameInfoCleanerTest >> testFrameReturnConvertedIntoCopy [

	| cfg t0 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block | 
		t0 := block frameReturn: 0 ].

	DRFrameInfoCleaner new applyTo: cfg.

	self deny: cfg instructions first isFrameReturn.
	self assert: cfg instructions first isCopy.
]

{ #category : 'tests' }
DRFrameInfoCleanerTest >> testLoadArgumentsNotUsedRemoved [

	| cfg anIndex t0|
	cfg := DRControlFlowGraph new.
	anIndex := 2.
	cfg newBasicBlockWith: [ :block |
		t0 := block loadMethodArgument: anIndex.].
	
	DRFrameInfoCleaner new applyTo: cfg.

	self deny: (cfg instructions includes: t0)
]

{ #category : 'tests' }
DRFrameInfoCleanerTest >> testLoadArgumentsUsedNotRemoved [

	| cfg t0 anIndex |
	cfg := DRControlFlowGraph new.
	anIndex := 2.
	cfg newBasicBlockWith: [ :block |
		t0 := block loadMethodArgument: anIndex .
		block return: t0 ].

	DRFrameInfoCleaner new applyTo: cfg.

	self assert: (cfg instructions includes: t0)
]
