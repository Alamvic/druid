Class {
	#name : #DRLoopInvariantCodeMotion,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRLoopInvariantCodeMotion >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph allBackJumps do: [ :backJump | 
		backJump backJumpTarget do: [ :target | 
			target phiFunctions
				select: [ :phi | phi isInvariantFrom: backJump basicBlock ]
				thenDo: [ :phi | 
					phi users do: [ :instruction | 
						self tryMove: instruction outsideLoopWith: backJump ] ] ] ]
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> tryMove: potentiallyLoopInvariantInstruction outsideLoopWith: backJump [

	| isInvariant loopPredecessors |
	isInvariant := potentiallyLoopInvariantInstruction operands 
		               allSatisfy: [ :operand | 
		               operand isInvariantFrom: backJump basicBlock ].

	isInvariant ifFalse: [ ^ self ].

	loopPredecessors := (backJump backJumpTarget flatCollect:
		                     #predecessors) asSet copyWithout:
		                    backJump basicBlock.
	self assert: [ loopPredecessors size = 1 ].
	
	"Should create pre-header block instead of moving instruction to previous block"
	potentiallyLoopInvariantInstruction moveToDominatingBlock: loopPredecessors anyOne.
	
	potentiallyLoopInvariantInstruction users do: [ :instruction | self tryMove: instruction outsideLoopWith: backJump ]
]
