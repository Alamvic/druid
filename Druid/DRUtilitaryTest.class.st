Class {
	#name : #DRUtilitaryTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #'as yet unclassified' }
DRUtilitaryTest >> testReplaceInstructionBy [

	|instruction1 instruction2 block|
	
	instruction1 := DRCopy new. 
	instruction2 := DRConstantValue new. 
	
	block := DRBasicBlock new. 
	block addInstruction: instruction1. 
	
	self assert: block instructions last isCopy. 
	
	block replaceInstruction: instruction1 by: instruction2. 
	
	self assert: block instructions last isConstant. 
]
