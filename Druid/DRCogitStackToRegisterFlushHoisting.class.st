Class {
	#name : #DRCogitStackToRegisterFlushHoisting,
	#superclass : #DROptimisation,
	#category : #'Druid-Cogit'
}

{ #category : #accessing }
DRCogitStackToRegisterFlushHoisting >> applyTo: cfg [
	"Insert a flush stack instruction in the common dominator of all those instructions that require stack flushing.
	Explain why we need to hoist it."

	| dominator instructionsNeedingStackFlush targetBlock |
	instructionsNeedingStackFlush := OrderedCollection new.
	cfg instructionsDo: [ :e |
		e needsStackFlush ifTrue: [ instructionsNeedingStackFlush add: e ] ].

	"If no instructions require flushing, do nothing"
	instructionsNeedingStackFlush ifEmpty: [ ^ self ].

	"Find the blocks that will be executed always"
	cfg invalidateDominatorTree.
	"We have to find the dominator of all instructions..."
	dominator := cfg dominatorTree dominatorOfAllBlocks:
		             (instructionsNeedingStackFlush collect: [ :e |
			              e basicBlock ]) , { cfg exitBasicBlock }.

	"... and add the flush to the closest dominator in the path to the end"
	targetBlock := dominator block.

	"BUT! If the target block has a conditional staged, we must follow the linearization at runtime"
	[
	targetBlock ~= dominator block and: [
		targetBlock endInstruction isStaged ] ] whileTrue: [
		targetBlock := targetBlock successors detect: [ :succ |
			               dominator block isDominatedBy: succ ] ].

	"If only one instruction requires to flush and it is in the target block, 
		the add it just before the instruction"
	(instructionsNeedingStackFlush size = 1 and: [
		 instructionsNeedingStackFlush first basicBlock = targetBlock ])
		ifTrue: [
			instructionsNeedingStackFlush first basicBlock addInstructionFirst:
				cfg instructionFactory flushStack.
			^ self ].

	"Add the flush at the end of the target block"
	targetBlock addInstruction: targetBlock instructionFactory flushStack
]
