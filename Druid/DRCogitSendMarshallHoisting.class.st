Class {
	#name : #DRCogitSendMarshallHoisting,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #accessing }
DRCogitSendMarshallHoisting >> applyTo: cfg [

	"This transformation copes with a limitation of Cogit: it does not properly handle the simulated stack on different execution branches.
	Since it executes linearly, if we pop on one branch, that will affect other branches (!!).
	This transformation does not solve the issue but prevents hitting the bug in simple cases.
	That is, if all branches pop the same number of elements, move the pops up in the CFG.
	We should take into account not only pops but also sends here, that *marshal* the stack"

	"Preparation: Split each JITMessageSend into a DRCogitSendMarshall and JITMessageSend instruction"
	"Then look for the common dominator of all marshall and pop instructions.
	Merge all of them in a single marshall instruction in the dominator, remove the others."
	
	| toMerge dominator marshall |
	cfg instructionsDo: [ :e |
		e isJITMessageSend ifTrue: [ 
			e addBefore: (DRCogitSendMarshall send: e)  ] ].
	
	toMerge := cfg instructions select: [ :e | e isPop or: [ e isCogitSendMarshall ] ].
	cfg buildDominatorTree.
	dominator := cfg tree dominatorOfAll: toMerge.
	
	marshall := toMerge detect: [ :e | e isCogitSendMarshall ].
	toMerge do: [ :e | e isCogitSendMarshall ifFalse: [ e removeFromCFG ]].

	marshall basicBlock removeInstruction: marshall.
	dominator block addInstruction: marshall
]
