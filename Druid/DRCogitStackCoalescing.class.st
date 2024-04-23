Class {
	#name : #DRCogitStackCoalescing,
	#superclass : #DROptimisation,
	#category : #'Druid-Cogit'
}

{ #category : #accessing }
DRCogitStackCoalescing >> applyTo: cfg [
	"Get all stack effect instructions (push and pops).
	We should move them down to their common post dominator.
	Effects that apply the same operation on different branches get coallesced into a single effect (one push on each branch turns into a single push down in the CFG)"

	| commonPostDominator basicBlock instructionsByStackDepth stackEffects |
	stackEffects := cfg instructions select: [ :e | e isStackEffect ].
	1 haltIf: [ stackEffects anySatisfy: [ :p | p hasUsers ] ].
	instructionsByStackDepth := stackEffects groupedBy: [ :effect |
		                            {effect stackDepth . effect class} ].
	instructionsByStackDepth keysAndValuesDo: [ :depth :instructionsAtDepth |
		instructionsAtDepth size > 1 ifTrue: [
			commonPostDominator := cfg postDominatorTree dominatorOfAll:
				                       instructionsAtDepth.
			basicBlock := commonPostDominator block.

			instructionsAtDepth anyOne isPush
				ifTrue: [
				self coalescePushes: instructionsAtDepth in: basicBlock ]
				ifFalse: [ self coalescePops: instructionsAtDepth in: basicBlock ] ] ]
]

{ #category : #accessing }
DRCogitStackCoalescing >> coalescePops: pops in: postDominator [

	postDominator addInstructionFirst:
		postDominator instructionFactory pop.

	pops do: [ :p | p removeFromCFG ]
]

{ #category : #accessing }
DRCogitStackCoalescing >> coalescePushes: pushes in: postDominator [
	"We have two pushes in two different branches
	
	   |           |
	Push R1     Push R2
	   \__________/
	         |
	        ...
	
	We should make a phi function between the two pushed values (R1, R2) and push that phi in the postdominator
	
	   |           |
	  ...         ...
	   \__________/
	         |
	    R3 = ø(R1,R2)
	    Push R3
	        ...
	"

	"Replace the push by a copy instruction.
	This simplifyies Phi creation because phis cannot contain constants
	"

	| copy copies phi |
	copies := pushes collect: [ :e |
		          copy := e instructionFactory copy: e operand1.
		          e replaceBy: copy.
		          copy ].

	phi := postDominator phiWithVariables: copies.
	postDominator addInstructionFirst:
		(postDominator instructionFactory push: phi)
]