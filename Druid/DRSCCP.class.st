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
		'cfgWorkList',
		'ssaWorkList'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #evaluating }
DRSCCP >> addJumpToCFGWorklist: aDRJump [

	^ edges detect: [ :e | 
		  e parent = (self getBlock: aDRJump) and: [ 
			  e child = aDRJump target ] ]
]

{ #category : #operations }
DRSCCP >> applyPropagation [

	| nonConstants |
	nonConstants := { self top. self bottom }.
	instancecfg instructionsDo: [ :i | 
		i operands withIndexDo: [ :operand :index | | lattice |
			lattice := self value: operand.
			(nonConstants includes: lattice) ifFalse: [
				i operandAt: index put: lattice asDRValue.
			] ].
		i isJump ifFalse: [ | lattice |
			lattice := self value: i.
			(nonConstants includes: lattice) ifFalse: [ 
				i basicBlock replaceInstruction: i by: (DRCopy
						 operands: { lattice asDRValue }
						 result: i result) ] ] ]
]

{ #category : #operations }
DRSCCP >> applyTo: cfg [


	self initializeSCCPWithCFG: cfg.

	[ cfgWorkList isEmpty and: [ssaWorkList isEmpty] ] whileFalse: [ 
		self stepCFGWorklist.
		self stepSSAWorklist].

	self applyPropagation.
]

{ #category : #'accessing - constants' }
DRSCCP >> bottom [

	^ 'Bot'
]

{ #category : #operations }
DRSCCP >> buildEdges: cfg [

	| list workList currentBlock edge|
	
	list := OrderedCollection new. 
	workList := OrderedCollection new. 
	workList add: cfg initialBasicBlock. 
	
	[workList isEmpty] whileFalse: [ 
		currentBlock := workList removeFirst. 
		currentBlock successors do: [ :s | 
			edge := DREdge new. 
			edge parent: currentBlock. 
			edge child: s. 
			list add: edge. 
			workList add: s]].
	
	^ list
	
]

{ #category : #operations }
DRSCCP >> dependenciesOf: anInstruction [
	
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

{ #category : #operations }
DRSCCP >> evaluateAssign: anInstruction [

	| previousValue newValue |
	
	anInstruction isJump ifTrue: [ 
		cfgWorkList add: (edges detect: [ :e |
			e parent = anInstruction basicBlock
				and: [ e child = anInstruction target ] ]).
		^ self ].
	anInstruction operands do: [ :o | self setValueOf: o at: (o sccpEvaluateFor: self) ].
	
	previousValue := self value: anInstruction. 
	self setValueOf: anInstruction at: (newValue := anInstruction sccpEvaluateFor: self).
	
	previousValue = newValue ifFalse: [
		ssaWorkList addAll: (self dependenciesOf: anInstruction) ]
]

{ #category : #operations }
DRSCCP >> evaluateConditionnal: aConditionalJump [

	| condition edge jumpTarget |
	condition := aConditionalJump condition sccpEvaluateFor: self.
	
	"If we know nothing, do nothing yet"
	condition = self top ifTrue: [ ^ self ].
	
	condition = self bottom ifTrue: [ 
		aConditionalJump targets do: [ :t | 
			edge := edges detect: [ :e | e child = t and: e parent = (self getBlock: aConditionalJump) ].
			cfgWorkList add: edge ].
		^ self ].
	
	jumpTarget := condition
		ifTrue: [ aConditionalJump trueBranch ]
		ifFalse: [ aConditionalJump falseBranch ].
	
	edge := edges detect: [ :e | 
		 e child = jumpTarget and:
				 e parent = (self getBlock: aConditionalJump) ].
	cfgWorkList add: edge.
]

{ #category : #operations }
DRSCCP >> evaluateInstruction: instruction [

	^ instruction isConditionalJump
		  ifFalse: [ self evaluateAssign: instruction ]
		  ifTrue: [ self evaluateConditionnal: instruction ]
]

{ #category : #operations }
DRSCCP >> evaluateOperands: aPhi [

	| edge |

	((self value: aPhi) ~= (self bottom)) ifTrue: [ 
		aPhi operands do: [ :r |
			 edge := edges select: [ :e | ((e parent) = (self getBlock: r)) and: ((e child) = (self getBlock: aPhi))]. 
			 edge := edge first.
			 edge isExecuted ifTrue: [
				edge child instructionsDo: [:i |
					i result = r ifTrue: [
						self setValueOf: r at: i ]]]]].
]

{ #category : #operations }
DRSCCP >> executedEdgesEntering: aBlock [

	^ edges select: [ :e | e child = aBlock and: [e isExecuted]].
]

{ #category : #operations }
DRSCCP >> getBlock: anInstruction [

	"Returns a block from a control flow graph that contains the instruction"
	^ anInstruction basicBlock
]

{ #category : #operations }
DRSCCP >> initialize [

	super initialize.
	latticeValues := OrderedDictionary new.
]

{ #category : #operations }
DRSCCP >> initializeLatticeValues: cfg [

	cfg instructionsDo: [ :i | latticeValues at: i put: self top ]
]

{ #category : #operations }
DRSCCP >> initializeSCCPWithCFG: cfg [

	instancecfg := cfg.
	ssaWorkList := OrderedCollection new.
	
	"This operation marks the edges as unexecuted."
	edges := OrderedCollection newFrom: (self buildEdges: cfg).

	"Edges leaving the initial block."
	cfgWorkList := OrderedCollection newFrom:
		               (edges select: [ :edge | 
			                edge parent = cfg initialBasicBlock ]).

	self initializeLatticeValues: cfg
]

{ #category : #operations }
DRSCCP >> interpretePhiFunction: operation [

	| incomingEdges executedIncomingEdges values |

	operation operands size = 0 ifTrue: [ 
		^ self value: operation ].
	
	incomingEdges := operation operands with: operation basicBlock predecessors collect: [ :operand :block | | edge |
		edge := edges detect: [ :potentialEdge | potentialEdge parent = block and: [ potentialEdge child = operation basicBlock ] ].
		{ edge . operand } ].
	
	executedIncomingEdges := incomingEdges select: [ :edgePair | edgePair first isExecuted ].

	values := executedIncomingEdges collect: [ :e | 
		self value: e second ] as: Set.
	values size = 1 ifTrue: [ ^ values anyOne ].
	^ self top
]

{ #category : #operations }
DRSCCP >> setValueOf: aRegister at: aValue [

	self haltIf: [ aValue isKindOf: DRAdd ].
	latticeValues at: aRegister put: aValue. 
]

{ #category : #operations }
DRSCCP >> stepCFGWorklist [

	| currentEdge edgeList |
	cfgWorkList isEmpty ifTrue: [ ^ self ].
	currentEdge := cfgWorkList removeFirst.
	currentEdge isExecuted ifTrue: [ ^ self ].

	currentEdge mark.

	edgeList := self executedEdgesEntering: currentEdge child.
	edgeList size > 1 ifTrue: [ ^ self ].

	currentEdge child instructionsDo: [ :instruction | 
		self evaluateInstruction: instruction ]
]

{ #category : #operations }
DRSCCP >> stepSSAWorklist [

	| currentInstruction workingBlock |
	ssaWorkList isEmpty ifTrue: [ ^ self ].

	currentInstruction := ssaWorkList removeFirst.
	workingBlock := self getBlock: currentInstruction.
	(self executedEdgesEntering: workingBlock) isEmpty ifFalse: [ 
		self evaluateInstruction: currentInstruction ]
]

{ #category : #'accessing - constants' }
DRSCCP >> top [

	^ 'Top'
]

{ #category : #operations }
DRSCCP >> value: aValue [

	"Return the lattice value for the register"
	(aValue isRegister and: [ aValue isVirtualRegister not ])
		ifTrue: [ ^ self bottom ].
	aValue isInstruction ifFalse: [ ^ aValue ].
	^ latticeValues at: aValue
]
