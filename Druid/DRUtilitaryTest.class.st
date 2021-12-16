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
