Class {
	#name : #DRLoopInvariantCodeMotion,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRLoopInvariantCodeMotion >> applyTo: aDRControlFlowGraph [

	"The current implementation of loop-invariant code motion has the following limitations:
	- it won't optimize loops that contain multiple basic blocks (e.g. loops that contain ifs)
	- when optimizing nested loops, it will only hoist loop-invariant instructions out of a single loop"

	aDRControlFlowGraph allLoops do: [ :loop |
		self moveAllInvariantLoopInstructionsUp: loop ]
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> moveAllInvariantLoopInstructionsUp: loop [

	self
		tryMoveAllInstructions: loop bodyInstructions
		outsideLoop: loop
		intoPreheader: nil
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> moveAllLoopInstructions: instructionsToMove outsideLoop: loop intoPreheader: preheader [

	| preheaderBlock |
	preheaderBlock := preheader ifNil: [ loop createLoopPreheaderBlockAndWrapWithIf ].
	instructionsToMove do: [ :i | 
		i moveToDominatingBlock: preheaderBlock ]
]

{ #category : #operations }
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
