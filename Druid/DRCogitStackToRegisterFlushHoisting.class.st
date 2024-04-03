Class {
	#name : #DRCogitStackToRegisterFlushHoisting,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #accessing }
DRCogitStackToRegisterFlushHoisting >> applyTo: cfg [
	"Insert a flush stack instruction in the common dominator of all those instructions that require stack flushing.
	Explain why we need to hoist it."

	| dominator instructionsNeedingStackFlush |
	instructionsNeedingStackFlush := OrderedCollection new.
	cfg instructionsDo: [ :e |
		e needsStackFlush ifTrue: [ instructionsNeedingStackFlush add: e ] ].

	"If no instructions require flushing, do nothing"
	instructionsNeedingStackFlush ifEmpty: [ ^ self ].

	"If only one instruction requires, flush just before the instruction"
	instructionsNeedingStackFlush size = 1 ifTrue: [
		instructionsNeedingStackFlush first addBefore:
			cfg instructionFactory flushStack.
		^ self ].

	cfg buildDominatorTree.
	dominator := (cfg dominatorTree dominatorOfAll:
		              instructionsNeedingStackFlush) block.
	dominator endInstruction addBefore:
		dominator instructionFactory flushStack
]
