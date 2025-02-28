Class {
	#name : 'DRStackEffectTest',
	#superclass : 'DRIRTest',
	#instVars : [
		'irBuilder'
	],
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'accessing' }
DRStackEffectTest >> addLoadStackValueAt: aValue [

	^ irBuilder
		addInstructionFrom: nil
		instructionKind: DRLoadStackValue
		operands: { aValue asDRValue }
]

{ #category : 'adding' }
DRStackEffectTest >> addLoadStackValueTop [

	^ self addLoadStackValueAt: 0
]

{ #category : 'adding' }
DRStackEffectTest >> addPop [

	^ irBuilder
		  addInstructionFrom: nil
		  instructionKind: DRPop
		  operands: #( 1 false )
]

{ #category : 'adding' }
DRStackEffectTest >> addPush: aValue [

	^ irBuilder
		addInstructionFrom: nil
		instructionKind: DRPush
		operands: { aValue asDRValue }
]

{ #category : 'tests' }
DRStackEffectTest >> setUp [ 

	irBuilder := DRMetaCompilerIRGenerator new.
	irBuilder pushFrame: (DRStackFrame new
		returnValue: 42 asDRValue;
		yourself).

]

{ #category : 'tests' }
DRStackEffectTest >> testInitialStackInstructionHasNoStackDependencies [

	| push pop |
	push := irBuilder addInstructionFrom: nil instructionKind: DRPush operands: { 17 }.
	pop := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	
	self assert: push stackDependencies isEmpty
]

{ #category : 'tests' }
DRStackEffectTest >> testLastStackInstructionHasNoStackDependents [

	| push pop |
	push := irBuilder addInstructionFrom: nil instructionKind: DRPush operands: { 17 }.
	pop := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	
	self assert: pop stackDependents isEmpty
]

{ #category : 'tests' }
DRStackEffectTest >> testPopDependsOnPush [

	| push pop |
	push := self addPush: 17.
	pop := self addPop.
	
	self assert: pop stackDependencies asArray equals: { push }.
	self assert: push stackDependents asArray equals: { pop }
]

{ #category : 'tests' }
DRStackEffectTest >> testPopInMergePointDependsOnBothBranches [

	| pop1 pop2 finalStackInstruction |
	pop1 := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	irBuilder buildIfTrue: nil then: [
		pop2 := irBuilder addInstructionFrom: nil instructionKind: DRPop ]. "Condition"

	finalStackInstruction := irBuilder
		                         addInstructionFrom: nil
		                         instructionKind: DRPush
		                         operands: { 17 }.

	self assert: finalStackInstruction stackDependencies asArray equals: {
			pop1.
			pop2 }
]

{ #category : 'tests' }
DRStackEffectTest >> testPopKnowsDefiningInstruction [

	| push pop push2 pop2 |
	push := self addPush: 17.
	push2 := self addPush: 18.
	
	pop := self addPop.
	pop2 := self addPop.

	self assert: pop definingStackInstruction equals: push2.
	self assert: pop2 definingStackInstruction equals: push
]

{ #category : 'tests' }
DRStackEffectTest >> testPushDependsOnPop [

	| push pop |
	pop := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	push := irBuilder addInstructionFrom: nil instructionKind: DRPush operands: { 17 }.

	self assert: push stackDependencies asArray equals: { pop }.
	self assert: pop stackDependents asArray equals: { push }
]

{ #category : 'tests' }
DRStackEffectTest >> testRebuildStackDependenciesReconstructsOriginalGraph [

	| pop1 pop2 finalStackInstruction |
	pop1 := self addPop.
	irBuilder buildIfTrue: nil then: [
		pop2 := self addPop. ].
	finalStackInstruction := self addPush: 17.

	irBuilder ir rebuildStackDependencies.

	self assert: finalStackInstruction stackDependencies asArray equals: { pop1. pop2 }.
	"Should keep the same order as predecessors"
	self assert: finalStackInstruction basicBlock predecessors asArray equals: { pop1 basicBlock . pop2 basicBlock }.

]

{ #category : 'tests' }
DRStackEffectTest >> testStackValueDoesNotUpdateWhenPopsAreMerged [

	|  pop1 pop2 loadTop |
	pop1 := self addPop.
	pop2 := self addPop.

	loadTop := self addLoadStackValueTop.

	self assert: loadTop definingStackInstruction equals: nil.
	self assert: loadTop operand1 equals: 0 asDRValue.
	
	"Merge the 2 pops into one, should not affect the other instructions"	
	DRCogitStackCanonicaliser new applyTo: irBuilder ir.
	
	self assert: loadTop definingStackInstruction equals: nil.
	self assert: loadTop operand1 equals: 0 asDRValue
]

{ #category : 'tests' }
DRStackEffectTest >> testStackValueKnowsDefiningInstruction [

	| push push2 loadPrev loadTop |
	push := self addPush: 17.
	push2 := self addPush: 18.

	loadPrev := self addLoadStackValueAt: 1.
	loadTop := self addLoadStackValueTop.

	self assert: loadPrev definingStackInstruction equals: push.
	self assert: loadTop definingStackInstruction equals: push2
]

{ #category : 'tests' }
DRStackEffectTest >> testStackValueLoseDefinitionWhenInstructionIsRemoved [

	| push loadTop |
	push := self addPush: 17.

	loadTop := self addLoadStackValueTop.

	self assert: loadTop definingStackInstruction equals: push.
	self assert: loadTop operand1 equals: 0 asDRValue.
	
	push removeFromCFG.

	self assert: loadTop definingStackInstruction equals: nil.
	self assert: loadTop operand1 equals: -1 asDRValue "Is this ok? Maybe check it in the validate?"
]

{ #category : 'tests' }
DRStackEffectTest >> testStackValueUpdatesWhenInstructionIsRemoved [

	| push push2 loadPrev |
	push := self addPush: 17.
	push2 := self addPush: 18.

	loadPrev := self addLoadStackValueAt: 1.

	self assert: loadPrev definingStackInstruction equals: push.
	self assert: loadPrev operand1 equals: 1 asDRValue.
	
	push2 removeFromCFG.

	self assert: loadPrev definingStackInstruction equals: push.
	self assert: loadPrev operand1 equals: 0 asDRValue.

]
