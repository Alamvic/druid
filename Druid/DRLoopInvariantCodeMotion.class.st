Class {
	#name : 'DRLoopInvariantCodeMotion',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'operations' }
DRLoopInvariantCodeMotion >> applyTo: aDRControlFlowGraph [

	"Note: when optimizing nested loops, the current implementation of loop-invariant code motion will only hoist loop-invariant instructions out of a single loop"

	aDRControlFlowGraph allLoops do: [ :loop | 
		self moveAllInvariantLoopInstructionsUp: loop ]
]

{ #category : 'operations' }
DRLoopInvariantCodeMotion >> moveAllInvariantLoopInstructionsUp: loop [

	self
		tryMoveAllInstructions: loop bodyInstructions
		outsideLoop: loop
		intoPreheader: nil
]

{ #category : 'operations' }
DRLoopInvariantCodeMotion >> moveAllLoopInstructions: instructionsToMove outsideLoop: loop intoPreheader: preheader [

	| preheaderBlock block |
	preheaderBlock := preheader ifNil: [
		                  block := loop
			                           createLoopPreheaderBlockAndWrapWithIf.
		                  loop controlFlowGraph invalidateDominatorTree.
		                  block ].
	instructionsToMove do: [ :i |
		i moveToDominatingBlock: preheaderBlock ]
]

{ #category : 'operations' }
DRLoopInvariantCodeMotion >> tryMoveAllInstructions: candidateInstructions outsideLoop: loop intoPreheader: preheader [

	| instructionsToMove |
	instructionsToMove := candidateInstructions select: [ :instruction | 
		                      instruction canMoveOutsideLoop: loop ].
	instructionsToMove ifEmpty: [ ^ self ].

	self
		moveAllLoopInstructions: instructionsToMove
		outsideLoop: loop
		intoPreheader: preheader.

	self
		tryMoveAllInstructions:
		(instructionsToMove flatCollect: [ :i | i users ])
		outsideLoop: loop
		intoPreheader: (preheader ifNil: [ loop preheaderBlock ])
]
