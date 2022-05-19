Class {
	#name : #DRComplexConditionalSplitting,
	#superclass : #DROptimisation,
	#instVars : [
		'currentJump'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRComplexConditionalSplitting >> applyTo: cfg [

	cfg allConditionalJumps
		select: [ :jump | jump condition isComplexCondition ]
		thenDo: [ :jump | jump acceptVisitor: self ]
]

{ #category : #visiting }
DRComplexConditionalSplitting >> splitComplexConditionOf: aJump between: targetSuccessor [

	| aComplexCondition newJump |
	aComplexCondition := aJump condition.
	aJump basicBlock controlFlowGraph newBasicBlockWith: [ :b | 
		aJump
			replaceOperand: aComplexCondition
			by: aComplexCondition operands first.

		newJump := b
			           jumpIf: aComplexCondition operands second
			           to: aJump trueBranch 
			           ifFalseTo: aJump falseBranch.

		targetSuccessor removePredecessor: aJump basicBlock.
		b addPredecessor: aJump basicBlock.
		aJump basicBlock replaceSuccessor: targetSuccessor by: b ].

	aJump condition isComplexCondition ifTrue: [ 
		aJump acceptVisitor: self ].
	newJump condition isComplexCondition ifTrue: [ 
		newJump acceptVisitor: self ]
]

{ #category : #visiting }
DRComplexConditionalSplitting >> visitAnd: aDRAnd [

	self
		splitComplexConditionOf: currentJump
		between: currentJump trueBranch
]

{ #category : #visiting }
DRComplexConditionalSplitting >> visitConditionalJump: aDRConditionalJump [

	currentJump := aDRConditionalJump.
	aDRConditionalJump condition acceptVisitor: self
]

{ #category : #visiting }
DRComplexConditionalSplitting >> visitOr: aDROr [

	self
		splitComplexConditionOf: currentJump
		between: currentJump falseBranch
]
