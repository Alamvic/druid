Class {
	#name : #DRCleanStackInstructionsTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #running }
DRCleanStackInstructionsTest >> setUp [

	super setUp.
	optimisation := DRCleanStackInstructions new
]

{ #category : #tests }
DRCleanStackInstructionsTest >> testReplaceComplexPushLoadPopSequence [

	| cfg push load pop push2 pop2 push3 pop3 |
	cfg := self setUpCFG: 1.

	push := cfg b1 push: 17.
	push2 := cfg b1 push: 18.
	push3 := cfg b1 push: 19.

	pop := cfg b1 pop.
	pop2 := cfg b1 pop.

	load := cfg b1 loadStackTop.

	pop3 := cfg b1 pop.

	optimisation applyTo: cfg.

	self assert: cfg instructions first isCopy.
	self assert: cfg instructions first operand value equals: 17.
	self assert: cfg instructions first result equals: load result.

	self denyCollection: cfg instructions includesAny: { push. push2. push3. load. pop. pop2. pop3 }
]

{ #category : #tests }
DRCleanStackInstructionsTest >> testReplaceManyPushLoadPopSequence [

	| cfg push load pop push2 load2 pop2 |
	cfg := self setUpCFG: 1.
	push := cfg b1 push: 17.
	push2 := cfg b1 push: 18.
	load := cfg b1 loadStackValueAt: 1.
	load2 := cfg b1 loadStackTop.
	pop := cfg b1 pop.
	pop2 := cfg b1 pop.

	optimisation applyTo: cfg.

	self assert: cfg instructions first isCopy.
	self assert: cfg instructions first operand value equals: 17.
	self assert: cfg instructions first result equals: load result.

	self assert: cfg instructions second isCopy.
	self assert: cfg instructions second operand value equals: 18.
	self assert: cfg instructions second result equals: load2 result.

	self denyCollection: cfg instructions includesAny: { push. push2. load. load2. pop. pop2 }
]

{ #category : #tests }
DRCleanStackInstructionsTest >> testReplacePushLoadPopSequence [

	| cfg push load pop |
	cfg := self setUpCFG: 1.
	push := cfg b1 push: 17.
	load := cfg b1 loadStackTop.
	pop := cfg b1 pop.

	optimisation applyTo: cfg.

	self assert: cfg instructions first isCopy.
	self assert: cfg instructions first operand value equals: 17.
	self assert: cfg instructions first result equals: load result.

	self denyCollection: cfg instructions includesAny: { push. load. pop }
]

{ #category : #tests }
DRCleanStackInstructionsTest >> testReplacePushLoadPopSequenceAfterMerge [

	| cfg push load pop push2 pop2 push3 pop3 |
	cfg := self setUpCFG: 1.

	push := cfg b1 push: 17.
	push2 := cfg b1 push: 18.
	push3 := cfg b1 push: 19.

	pop := cfg b1 pop.
	pop2 := cfg b1 pop.

	load := cfg b1 loadStackTop.

	pop3 := cfg b1 pop.
	
	"Merge the 2 pops into one, should not affect the other instructions"	
	DRCogitStackCanonicaliser new applyTo: cfg.

	optimisation applyTo: cfg.

	self assert: cfg instructions first isNoop. "Because of merge"
	self assert: cfg instructions second isCopy.
	self assert: cfg instructions second operand value equals: 17.
	self assert: cfg instructions second result equals: load result.

	self denyCollection: cfg instructions includesAny: { push. push2. push3. load. pop. pop2. pop3 }
]

{ #category : #tests }
DRCleanStackInstructionsTest >> testReplaceSomePushLoadPopSequence [

	| cfg push load pop push2 pop2 |
	cfg := self setUpCFG: 1.
	push := cfg b1 push: 17.
	push2 := cfg b1 push: 18.
	load := cfg b1 loadStackTop.
	pop := cfg b1 pop.
	pop2 := cfg b1 pop.

	optimisation applyTo: cfg.

	self assert: cfg instructions first isCopy.
	self assert: cfg instructions first operand value equals: 18.
	self assert: cfg instructions first result equals: load result.

	self denyCollection: cfg instructions includesAny: { push. push2. load. pop. pop2 }
]
