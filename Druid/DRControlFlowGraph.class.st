Class {
	#name : #DRControlFlowGraph,
	#superclass : #Object,
	#instVars : [
		'initialBasicBlock',
		'exitBasicBlock'
	],
	#category : #'Druid-IR'
}

{ #category : #iterating }
DRControlFlowGraph >> blocks [

	"Return the list of blocks based on a simple linearization algorithm"

	| blocks |
	blocks := OrderedCollection new.
	self blocksDo: [ :b | blocks add: b ].
	^ blocks
]

{ #category : #iterating }
DRControlFlowGraph >> blocksDo: aFullBlockClosure [ 

	| worklist visited |
	visited := Set new.
	worklist := OrderedCollection new.
	worklist add: initialBasicBlock.
	
	[ worklist isEmpty ] whileFalse: [ | currentBlock |
		currentBlock := worklist removeFirst.
		aFullBlockClosure value: currentBlock.
		currentBlock instructions notEmpty ifTrue: [
			currentBlock lastInstruction targets do: [ :target |
				(visited includes: target) ifFalse: [ 
					worklist addLast: target.
					visited add: target ] ] ] ]
]

{ #category : #accessing }
DRControlFlowGraph >> exitBasicBlock: aDRBasicBlock [ 
	exitBasicBlock := aDRBasicBlock
]

{ #category : #accessing }
DRControlFlowGraph >> initialBasicBlock: aDRBasicBlock [ 
	initialBasicBlock := aDRBasicBlock
]

{ #category : #accessing }
DRControlFlowGraph >> instructions [
	
	| instructions |
	instructions := OrderedCollection new.
	self instructionsDo: [ :e |
		instructions add: e ].
	^ instructions
]

{ #category : #iterating }
DRControlFlowGraph >> instructionsDo: aFullBlockClosure [ 

	| worklist visited |
	visited := Set new.
	worklist := OrderedCollection new.
	worklist add: initialBasicBlock.
	
	[ worklist isEmpty ] whileFalse: [ | currentBlock |
		currentBlock := worklist removeFirst.
		currentBlock instructionsDo: [ :e |
			aFullBlockClosure value: e.
			e targets do: [ :target |
				1haltIf: [ target isNil ].
				(visited includes: target) ifFalse: [ 
					worklist add: target.
					visited add: target ] ] ] ]
]
