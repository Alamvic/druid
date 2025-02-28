Class {
	#name : 'DRCogitStackCoalescing',
	#superclass : 'DRStackOptimisation',
	#category : 'Druid-Cogit',
	#package : 'Druid',
	#tag : 'Cogit'
}

{ #category : 'accessing' }
DRCogitStackCoalescing >> coalescePops: pops in: postDominator [
	"If two pops have different spilling value, we need to unspill the spilled one"

	(pops collect: [ :e | e isUnspilled ] as: Set) size > 1 ifTrue: [
		pops reject: [ :e | e isUnspilled ] thenDo: [ :e |
			e addBefore: (e instructionFactory unspill: (e stackDepth + 1) negated) ] ].


	postDominator addInstructionFirst: (postDominator instructionFactory
			 pop: 1
			 unspilled: (pops anySatisfy: [ :e | e isUnspilled ])).
	pops do: [ :p | p removeFromCFG ]
]

{ #category : 'accessing' }
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

{ #category : 'accessing' }
DRCogitStackCoalescing >> doApply: cfg [
	"Get all stack effect instructions (push and pops).
	We should move them down to their common post dominator.
	Effects that apply the same operation on different branches get coallesced into a single effect (one push on each branch turns into a single push down in the CFG)"

	| commonPostDominator basicBlock stackEffects currentStackEffects stackEffectsInReverseOrder previous |
	stackEffects := cfg instructions select: [ :e | e isStackEffect ].

	stackEffectsInReverseOrder := Stack new.
	currentStackEffects := #(  ).
	currentStackEffects := stackEffects select: [ :e |
		                       (stackEffects intersection:
			                        e stackEffectDependencies) isEmpty ].

	previous := nil.
	[ stackEffects notEmpty and: [ stackEffects ~= previous ] ] whileTrue: [
		previous := stackEffects copy.
		stackEffects removeAll: currentStackEffects.
		currentStackEffects size > 1 ifTrue: [
			stackEffectsInReverseOrder push: currentStackEffects ].
		currentStackEffects := stackEffects select: [ :e |
			                       (stackEffects intersection:
				                        e stackEffectDependencies) isEmpty ] ].

	[ stackEffectsInReverseOrder isEmpty ] whileFalse: [
		currentStackEffects := stackEffectsInReverseOrder pop.
		commonPostDominator := cfg postDominatorTree dominatorOfAll:
			                       currentStackEffects.
		basicBlock := commonPostDominator block.

		currentStackEffects anyOne isPush
			ifTrue: [ self coalescePushes: currentStackEffects in: basicBlock ]
			ifFalse: [ self coalescePops: currentStackEffects in: basicBlock ] ]
]
