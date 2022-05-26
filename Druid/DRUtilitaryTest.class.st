Class {
	#name : #DRUtilitaryTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #'as yet unclassified' }
DRUtilitaryTest >> testReplaceInstructionBy [

	|instruction1 instruction2 block cfg |
	
	instruction1 := DRCopy new.
	instruction2 := DRAdd new. 
	
	cfg := DRControlFlowGraph new.
	block := cfg newBasicBlock.
	block addInstruction: instruction1. 
	
	self assert: block instructions allButLast last isCopy. 
	
	block replaceInstruction: instruction1 by: instruction2. 
	
	self assert: block instructions allButLast last isAdd.
]

{ #category : #'as yet unclassified' }
DRUtilitaryTest >> testReversePostOrderOfLoop [

	| cfg b0 loopHeader loopBody exit |
	
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	loopHeader := cfg newBasicBlock.
	loopBody := cfg newBasicBlock.
	exit := cfg newBasicBlock.
	
	b0 jumpTo: loopHeader.
	loopHeader jumpIfTrueTo: loopBody ifFalseTo: exit.
	loopBody backJumpTo: loopHeader.
	
	self assert: cfg reversePostOrderBlocks asArray equals: { b0 . loopHeader . loopBody . exit}
]
