Class {
	#name : #DRStackEffectTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRStackEffectTest >> testInitialStackInstructionHasNoStackDependencies [

	| irBuilder push pop |
	irBuilder := DRMetaCompilerIRGenerator new.
	irBuilder pushFrame: DRStackFrame new.

	push := irBuilder addInstructionFrom: nil instructionKind: DRPush operands: { 17 }.
	pop := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	
	self assert: push stackDependency isEmpty
]

{ #category : #tests }
DRStackEffectTest >> testLastStackInstructionHasNoStackDependents [

	| irBuilder push pop |
	irBuilder := DRMetaCompilerIRGenerator new.
	irBuilder pushFrame: DRStackFrame new.

	push := irBuilder addInstructionFrom: nil instructionKind: DRPush operands: { 17 }.
	pop := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	
	self assert: pop stackDependents isEmpty
]

{ #category : #tests }
DRStackEffectTest >> testPopDependsOnPush [

	| irBuilder push pop |
	irBuilder := DRMetaCompilerIRGenerator new.
	irBuilder pushFrame: DRStackFrame new.

	push := irBuilder addInstructionFrom: nil instructionKind: DRPush operands: { 17 }.
	pop := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	
	self assert: pop stackDependency asArray equals: { push }.
	self assert: push stackDependents asArray equals: { pop }
]

{ #category : #tests }
DRStackEffectTest >> testPopInMergePointDependsOnBothBranches [

	| irBuilder pop1 pop2 finalStackInstruction |
	irBuilder := DRMetaCompilerIRGenerator new.
	irBuilder pushFrame: (DRStackFrame new
		returnValue: DRPhiFunction new;
		yourself).


	pop1 := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	irBuilder buildIfTrue: nil then: [
		pop2 := irBuilder addInstructionFrom: nil instructionKind: DRPop ]. "Condition"

	finalStackInstruction := irBuilder
		                         addInstructionFrom: nil
		                         instructionKind: DRPush
		                         operands: { 17 }.

	self assert: finalStackInstruction stackDependency asArray equals: {
			pop1.
			pop2 }
]

{ #category : #tests }
DRStackEffectTest >> testPushDependsOnPop [

	| irBuilder push pop |
	irBuilder := DRMetaCompilerIRGenerator new.
	irBuilder pushFrame: DRStackFrame new.

	pop := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	push := irBuilder addInstructionFrom: nil instructionKind: DRPush operands: { 17 }.

	self assert: push stackDependency asArray equals: { pop }.
	self assert: pop stackDependents asArray equals: { push }
]

{ #category : #tests }
DRStackEffectTest >> testRebuildStackDependenciesReconstructsOriginalGraph [

	| irBuilder pop1 pop2 finalStackInstruction |
	irBuilder := DRMetaCompilerIRGenerator new.
	irBuilder pushFrame: (DRStackFrame new
		returnValue: DRPhiFunction new;
		yourself).


	pop1 := irBuilder addInstructionFrom: nil instructionKind: DRPop.
	irBuilder buildIfTrue: nil then: [
		pop2 := irBuilder addInstructionFrom: nil instructionKind: DRPop ]. "Condition"

	finalStackInstruction := irBuilder
		                         addInstructionFrom: nil
		                         instructionKind: DRPush
		                         operands: { 17 }.

	irBuilder ir rebuildStackDependencies.
	self assert: finalStackInstruction stackDependency asArray equals: {
			pop1.
			pop2 }
]
