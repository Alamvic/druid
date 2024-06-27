Class {
	#name : #DRCogitStackToRegisterFlushHoisting,
	#superclass : #DROptimisation,
	#category : #'Druid-Cogit'
}

{ #category : #accessing }
DRCogitStackToRegisterFlushHoisting >> applyTo: cfg [
	"Insert a flush stack instruction in the common dominator of all those instructions that require stack flushing.
	Explain why we need to hoist it."

	| dominator flushStackInstructions targetBlock |
	flushStackInstructions := OrderedCollection new.
	cfg instructionsDo: [ :e |
		e isFlushStack ifTrue: [ flushStackInstructions add: e ] ].

	"If no instructions require flushing, do nothing"
	flushStackInstructions ifEmpty: [ ^ self ].

	"Find the blocks that will be executed always"
	cfg invalidateDominatorTree.
	"We have to find the dominator of all instructions..."
	dominator := cfg dominatorTree dominatorOfAllBlocks:
		             (flushStackInstructions collect: [ :e |
			              e basicBlock ]) , cfg exitBasicBlocks.

	"... and add the flush to the closest dominator in the path to the end"
	targetBlock := dominator block.

	"BUT! If the target block has a conditional staged, we must follow the linearization at runtime"
	[
	targetBlock ~= dominator block and: [
		targetBlock endInstruction isStaged ] ] whileTrue: [
		targetBlock := targetBlock successors detect: [ :succ |
			               dominator block isDominatedBy: succ ] ].

	flushStackInstructions do: [ :e | e removeFromCFG ].

	"If only one instruction requires to flush and it is in the target block, 
		the add it just before the instruction"
	(flushStackInstructions size = 1 and: [
		 flushStackInstructions first basicBlock = targetBlock ])
		ifTrue: [
			flushStackInstructions first basicBlock addInstructionFirst:
				flushStackInstructions first.
			^ self ].

	"Add the flush at the end of the target block"
	targetBlock addInstruction: flushStackInstructions first
]
