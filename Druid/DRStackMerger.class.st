Class {
	#name : #DRStackMerger,
	#superclass : #Object,
	#instVars : [
		'mergedState',
		'mergeBlock',
		'builder'
	],
	#category : #'Druid-Tests'
}

{ #category : #accessing }
DRStackMerger >> builder [

	^ builder
]

{ #category : #accessing }
DRStackMerger >> builder: anObject [

	builder := anObject
]

{ #category : #merging }
DRStackMerger >> freshPhiFunction [
	
	^ self instructionFactory phiWithVariables: OrderedCollection new
]

{ #category : #merging }
DRStackMerger >> installInstructions [

	mergedState executionStack framesDo: [ :mergeFrame | 
		mergeFrame temporariesDo: [ :e | mergeBlock addInstruction: e ] ].

	mergedState vmOperandStack do: [ :e | mergeBlock addInstruction: e ].
	mergeBlock addInstruction: mergedState primitiveFailureCode
]

{ #category : #merging }
DRStackMerger >> instructionFactory [

	^ mergeBlock controlFlowGraph instructionFactory
]

{ #category : #merging }
DRStackMerger >> mergeAll: aCollectionOfExecutionStates [

	| stacks mergedStack mergedVMState vmStates |
	vmStates := aCollectionOfExecutionStates collect: [ :e | e vmState ].
	stacks := aCollectionOfExecutionStates collect: [ :e | 
		          e executionStack ].
	mergedVMState := self mergeVMStates: vmStates.
	mergedStack := self mergeStacks: stacks simplifying: true.
	^ mergedState := DRExecutionState new
		                 vmState: mergedVMState;
		                 executionStack: mergedStack;
		                 yourself
]

{ #category : #merging }
DRStackMerger >> mergeAll: aCollectionOfExecutionStates upTo: aFrame [

	| stacks mergedStack mergedVMState vmStates |
	vmStates := aCollectionOfExecutionStates collect: [ :e | e vmState ].
	stacks := aCollectionOfExecutionStates collect: [ :e | 
		          e executionStack ].
	mergedVMState := self mergeVMStates: vmStates.
	mergedStack := self mergeStacks: stacks simplifying: true.
	
	mergedState := DRExecutionState new
		  vmState: mergedVMState;
		  executionStack: mergedStack;
		  yourself.
	mergedState popUpTo: aFrame.
	self installInstructions.
	^ mergedState
]

{ #category : #accessing }
DRStackMerger >> mergeBlock [

	^ mergeBlock
]

{ #category : #accessing }
DRStackMerger >> mergeBlock: anObject [

	mergeBlock := anObject
]

{ #category : #merging }
DRStackMerger >> mergeFramesIn: stacksToMerge at: anInteger [ 
	
	"If I have no predecessors, I'm very probably dead code.
	No need to merge anything"
	| mergeFrame prototype |
	prototype := stacksToMerge anyOne at: anInteger.
	mergeFrame := prototype copy.
	
	"Initialize the merge frame with phi functions"
	mergeFrame temporaryNames do: [ :name | 
		mergeFrame
			writeTemporaryNamed: name
			withValue: self freshPhiFunction ].
	
	stacksToMerge do: [ :s | | frameToMerge |
		frameToMerge := s "a DRExecutionStack"
			frameAt: anInteger
			ifAbsent: [ | nullFrame |
				nullFrame := DRStackFrame new.
				mergeFrame temporaryNames do: [ :n |
					nullFrame
						writeTemporaryNamed: n
						withValue: DRNullValue new
				].
				nullFrame ].
		frameToMerge addTo: mergeFrame.
	].
	
	^ mergeFrame
]

{ #category : #merging }
DRStackMerger >> mergeStacks: stacksToMerge simplifying: aBoolean [ 
		
	| mergedExecutionStack maxStackDepth |
	maxStackDepth := stacksToMerge max: [ :e | e size ].

	mergedExecutionStack := DRExecutionStack new.
	"Iterate backwards so we push them back the the proper order"
	(maxStackDepth to: 1 by: -1) do: [ :index | 
		mergedExecutionStack pushMergeFrame: (self mergeFramesIn: stacksToMerge at: index) ].
	^ mergedExecutionStack
]

{ #category : #merging }
DRStackMerger >> mergeVMStates: vmStates [ 
	
	| mergedVMState |
	self
		assert: (vmStates collect: [ :e | e operandStack size ]) asSet size = 1
		description: 'All vm states should have the same depth'.
	
	mergedVMState := builder newVMState.
	mergedVMState primitiveFailureCode: self freshPhiFunction.
	vmStates do: [ :e | 
		e addTo: mergedVMState ].
	
	^ mergedVMState
]
