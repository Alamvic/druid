"
This class is an available optimisations for Druid.

It is called on DRControlFlowGraph.

It executes folding on the operations when they contain constants, which reduces the number of useful code lines and allows dead code elimination, as well as taking into account the conditionnal jumps of the control flow graph. If some conditions can be evaluated at compile time, the blocks that won't be called are not optimized and the phi functions that contain variables from these blocks are resolved. It includes constant folding and constant propagation. 

The replacement values are firstly kept in a lattice while the computations are done. When the algorithm has finished exploring the graph, the replacement is done. 

This is the final version of SCCP. This algorithm is meant to be called on use cases. 

More information about SCCP algorithm : 
http://www.r-5.org/files/books/computers/compilers/writing/Keith_Cooper_Linda_Torczon-Engineering_a_Compiler-EN.pdf
Section 10.7

The tests use a special class named DRControlFlowGraphForTesting that has the same functionnalities and behavior as DRControlFlowGraph, but adds accessors and let the test set up be easier. 

[Note] : In the book above, the algorithm is built to optimise control flow graphs that contain only one operation by block (assign or conditionnal). This is not true for Druid, so some parts of the algorithm had to be changed to take this difference into account. 
The algorithm was also refactored to fit Pharo's object model, so they are intermediate methods that are not in the book that were implemented. 
Some parts were also divided into several methods for readability. 
The CFGEdges were modeled by the DREdge objects. 
The SSAEdges were replaced by the exiting instruction instead. 
"
Class {
	#name : #DRSCCP,
	#superclass : #Object,
	#instVars : [
		'latticeValues',
		'edges',
		'instancecfg',
		'ssaWorkList',
		'cfgEdgeWorkList'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #executing }
DRSCCP >> addJumpToCFGWorklist: aDRJump [

	"Add to the edge work list the edge corresponding to this jump instruction"
	cfgEdgeWorkList add: (edges detect: [ :e | 
		  e source = aDRJump basicBlock and: [ 
			  e destination = aDRJump target ] ])
]

{ #category : #executing }
DRSCCP >> addToEdgeCFGWorklistConditionalJump: aDRJump targetting: aBasicBlock [

	"Add to the edge work list the edge corresponding to this jump instruction"

	| edge |
	edge := edges detect: [ :e | 
		        e destination = aBasicBlock and: [ 
			        e source = aDRJump basicBlock ] ].
	cfgEdgeWorkList add: edge
]

{ #category : #executing }
DRSCCP >> applyPropagation [

	| nonConstants |
	nonConstants := { self top. self bottom }.
	instancecfg instructionsDo: [ :i | 
		i operands withIndexDo: [ :operand :index | | lattice |
			lattice := self latticeValue: operand.
			(nonConstants includes: lattice) ifFalse: [
				i operandAt: index put: lattice asDRValue.
			] ].
		i isJump ifFalse: [ | lattice |
			lattice := self latticeValue: i.
			(nonConstants includes: lattice) ifFalse: [ 
				i basicBlock replaceInstruction: i by: (DRCopy
						 operands: { lattice asDRValue }
						 result: i result) ] ] ]
]

{ #category : #executing }
DRSCCP >> applyTo: cfg [


	self initializeSCCPWithCFG: cfg.

	[ cfgEdgeWorkList isEmpty and: [ssaWorkList isEmpty] ] whileFalse: [ 
		self stepCFGEdgeWorklist.
		self stepInstructionWorklist].

	self applyPropagation.
]

{ #category : #lattice }
DRSCCP >> bottom [

	^ 'Bot'
]

{ #category : #initialization }
DRSCCP >> buildEdges: cfg [

	| list workList currentBlock edge|
	
	list := OrderedCollection new. 
	workList := OrderedCollection new. 
	workList add: cfg initialBasicBlock. 
	
	[workList isEmpty] whileFalse: [ 
		currentBlock := workList removeFirst. 
		currentBlock successors do: [ :s | 
			edge := DREdge new. 
			edge source: currentBlock. 
			edge destination: s. 
			list add: edge. 
			workList add: s]].
	
	^ list
]

{ #category : #executing }
DRSCCP >> evaluateInstruction: instruction [

	instruction sccpEvaluateFor: self
]

{ #category : #querying }
DRSCCP >> executedEdgesEntering: aBlock [

	^ edges select: [ :e | e destination = aBlock and: [e isExecuted]].
]

{ #category : #initialization }
DRSCCP >> initialize [

	super initialize.
	latticeValues := Dictionary new.
	ssaWorkList := OrderedCollection new.
	cfgEdgeWorkList := OrderedCollection new.
]

{ #category : #initialization }
DRSCCP >> initializeSCCPWithCFG: cfg [

	instancecfg := cfg.
	
	"This operation marks the edges as unexecuted."
	edges := self buildEdges: cfg.

	"Edges leaving the initial block."
	cfgEdgeWorkList addAll: (edges select: [ :edge | edge source = cfg initialBasicBlock ])
]

{ #category : #executing }
DRSCCP >> interpretePhiFunction: operation [

	| incomingEdges executedIncomingEdges values |

	values := operation operands collect: [ :e | 
		self latticeValue: e ] as: Set.
	values remove: self top ifAbsent: [ "Nothing" ].
	values size = 1 ifTrue: [ ^ values anyOne ].
	^ self bottom
]

{ #category : #lattice }
DRSCCP >> latticeValue: aValue [

	"Return the lattice value for the register"
	(aValue isRegister and: [ aValue isVirtualRegister not ])
		ifTrue: [ ^ self bottom ].
	aValue isInstruction ifFalse: [ ^ aValue ].

	^ latticeValues at: aValue ifAbsent: [ self top ]
]

{ #category : #lattice }
DRSCCP >> setValueOf: anInstruction at: aValue [

	"Set the new lattice value associated with the instruction.
	Invalidate all its usages so they are recomputed"
	latticeValues at: anInstruction put: aValue.
	ssaWorkList addAll: (self usesOf: anInstruction)
]

{ #category : #executing }
DRSCCP >> stepCFGEdgeWorklist [

	"Each basic block is visited only once.
	This means that edges are visited only once, and blocks with many predecessors are only visited on their first visit"
	
	| currentEdge edgeList |

	cfgEdgeWorkList isEmpty ifTrue: [ ^ self ].

	currentEdge := cfgEdgeWorkList removeFirst.
	currentEdge isExecuted ifTrue: [ ^ self ].
	
	"Mark the current edge as executed"
	currentEdge mark.

	"If there are more than one edge executed arriving to this block already, this means that we already visited it before.
	Quit"
	edgeList := self executedEdgesEntering: currentEdge destination.
	edgeList size > 1 ifTrue: [ ^ self ].

	currentEdge destination instructionsDo: [ :instruction | 
		self evaluateInstruction: instruction ]
]

{ #category : #executing }
DRSCCP >> stepInstructionWorklist [

	| currentInstruction workingBlock |
	ssaWorkList isEmpty ifTrue: [ ^ self ].

	currentInstruction := ssaWorkList removeFirst.
	workingBlock := currentInstruction basicBlock.
	(self executedEdgesEntering: workingBlock) isEmpty ifFalse: [ 
		self evaluateInstruction: currentInstruction ]
]

{ #category : #lattice }
DRSCCP >> top [

	^ 'Top'
]

{ #category : #querying }
DRSCCP >> usesOf: anInstruction [
	
	| ret |

	anInstruction isConditionalJump ifTrue: [
		self value = self top ifTrue: [ 
			ret := OrderedCollection new. 
			ret addAll: anInstruction trueBranch instructions.
			ret addAll: anInstruction falseBranch instructions. 
			^ ret ]
		ifFalse: [ 
			"We assume self value = self bottom" 
			anInstruction condition value ifTrue: [ 
				 ret := OrderedCollection new. 
				 ret addAll: anInstruction trueBranch instructions.
				^ ret. ]
			ifFalse: [ 
				ret := OrderedCollection new. 
				ret addAll: anInstruction falseBranch instructions.
				^ ret. ] ] ]. 
	^ instancecfg instructions select: [ :instruction | 
		  instruction operands includes: anInstruction ]
]
