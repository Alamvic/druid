Class {
	#name : 'DRStackMerger',
	#superclass : 'Object',
	#instVars : [
		'mergedState',
		'mergeBlock',
		'builder'
	],
	#category : 'Druid-CompilerBuilder',
	#package : 'Druid',
	#tag : 'CompilerBuilder'
}

{ #category : 'accessing' }
DRStackMerger >> builder [

	^ builder
]

{ #category : 'accessing' }
DRStackMerger >> builder: anObject [

	builder := anObject
]

{ #category : 'merging' }
DRStackMerger >> freshPhiFunction [
	
	^ self instructionFactory phiWithVariables: OrderedCollection new
]

{ #category : 'merging' }
DRStackMerger >> installInstructions [

	mergedState executionStack framesDo: [ :mergeFrame | 
		mergeFrame temporariesDo: [ :e | mergeBlock addInstruction: e ].
		mergeFrame operandStack do: [ :e | mergeBlock addInstruction: e ].
		mergeBlock addInstruction: mergeFrame returnValue ].
	mergedState vmOperandStack do: [ :e | mergeBlock addInstruction: e ].
	self flag: #OnlyPrimitives.
	mergedState primitiveFailureCode operands ifNotEmpty: [ 
		mergeBlock addInstruction: mergedState primitiveFailureCode ]
]

{ #category : 'merging' }
DRStackMerger >> instructionFactory [

	^ mergeBlock controlFlowGraph instructionFactory
]

{ #category : 'merging' }
DRStackMerger >> mergeAll: aCollectionOfExecutionStates [

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
	self installInstructions.
	^ mergedState
]

{ #category : 'merging' }
DRStackMerger >> mergeAll: aCollectionOfExecutionStates upTo: aFrame [

	^ self mergeAll: (aCollectionOfExecutionStates collect: [ :e | e popUpTo: aFrame ])
]

{ #category : 'accessing' }
DRStackMerger >> mergeBlock [

	^ mergeBlock
]

{ #category : 'accessing' }
DRStackMerger >> mergeBlock: anObject [

	mergeBlock := anObject
]

{ #category : 'merging' }
DRStackMerger >> mergeFramesIn: stacksToMerge at: anInteger [ 
	
	"If I have no predecessors, I'm very probably dead code.
	No need to merge anything"
	| mergeFrame prototype |
	
	((stacksToMerge collect: [ :s | (s frameAt: anInteger) operandStack size ] as: Set) size > 1)
		ifTrue: [ self error ].
	
	prototype := stacksToMerge anyOne at: anInteger.
	mergeFrame := prototype copy.
	
	"Initialize the merge frame with phi functions"
	mergeFrame temporaryNames do: [ :name | 
		mergeFrame
			writeTemporaryNamed: name
			withValue: self freshPhiFunction ].
	mergeFrame operandStack removeAll.
	1 to: prototype operandStack size do: [ :i |
		mergeFrame push: self freshPhiFunction ].
	mergeFrame returnValue: self freshPhiFunction.
	
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

{ #category : 'merging' }
DRStackMerger >> mergeStacks: stacksToMerge simplifying: aBoolean [ 
		
	| mergedExecutionStack maxStackDepth |
	maxStackDepth := stacksToMerge max: [ :e | e size ].

	mergedExecutionStack := DRExecutionStack new.
	"Iterate backwards so we push them back the the proper order"
	1 to: maxStackDepth do: [ :index | 
		mergedExecutionStack pushMergeFrame: (self
			mergeFramesIn: stacksToMerge
			at: maxStackDepth - index + 1) ].
	^ mergedExecutionStack
]

{ #category : 'merging' }
DRStackMerger >> mergeVMStates: vmStates [ 
	
	| mergedVMState maxVMStackDepth |
	maxVMStackDepth := vmStates max: [ :e | e operandStack size ].
	mergedVMState := builder newVMState.
	mergedVMState primitiveFailureCode: self freshPhiFunction.
	vmStates do: [ :e |
		e addTo: mergedVMState depth: maxVMStackDepth ].
	^ mergedVMState
]
