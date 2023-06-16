"
I am a Druid optimisation that implements the SCCP algorithm (Sparce Conditional Constant Propagation) on IR instructions.

I propagate constants in the control flow graph instructions and perform constant folding on the instructions when I determine they operands will be constant.
This reduces the number of useful lines of code and allows a subsequent pass of dead code elimination.

Moreover, I apply constant folding and propagation conditionally: edges in the control flow graph are visited only if I determine that they can be visited at runtime.
On the contrary, if I can statically decide that some edge will never be visited, e.g., if a condition is always false, those edges will never be visited and allow more agressive propagations. 

I am implemented as a two-phase algorithm.
The first phase does an abstract interpretation of the control flow graph and computes the lattice values that correspond to each instruction in the graph.
The second phase replaces
 - each instruction with a constant lattice value by a copy instruction with that value
 - each operand with a constant lattive value by a that value

More information about SCCP algorithm : 
http://www.r-5.org/files/books/computers/compilers/writing/Keith_Cooper_Linda_Torczon-Engineering_a_Compiler-EN.pdf
Section 10.7

[Note]: In the book above, the algorithm is built to optimise control flow graphs that contain only one operation per block (assign or conditionnal).
This is not true for Druid, so some parts of the algorithm had to be changed to take this difference into account. 
The algorithm was also refactored for readability, so there are intermediate methods that are not in the book. 

The CFGEdges are modelled by the DREdge objects. 
The SSAEdges are modelled by the instruction itself, because Druid IR is in SSA form. 
"
Class {
	#name : #DRSCCP,
	#superclass : #DRControlFlowOptimisation,
	#instVars : [
		'latticeValues',
		'edges',
		'instancecfg',
		'ssaWorkList',
		'cfgEdgeWorkList',
		'edgesByDestination'
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

	instancecfg instructions copy do: [ :i |
		"Replace instructions and instruction operands using the computed lattices"
		self applyPropagationToInstruction: i ]
]

{ #category : #executing }
DRSCCP >> applyPropagationToInstruction: i [

	"Replace the operands of instructions by their propagated values on non phi functions"
	self tryReplaceInstructionOperands: i.
	self tryReplaceConditionalJump: i.
	self tryReplaceInstructionByFoldedValue: i.

]

{ #category : #lattice }
DRSCCP >> bottom [

	^ 'Bot'
]

{ #category : #initialization }
DRSCCP >> buildEdges: cfg [

	| list workList currentBlock edge done |
	
	list := OrderedCollection new.
	done := Set new.
	workList := OrderedCollection new. 
	workList add: cfg initialBasicBlock. 
	
	[workList isEmpty] whileFalse: [ 
		currentBlock := workList removeFirst. 
		currentBlock successors do: [ :s | 
			edge := DRSCCPEdge withSource: currentBlock andDestination: s.
			
			"Pre-compute the set of edges per destination.
			This is an optimisation for large graphs"
			(edgesByDestination
				at: s
				ifAbsentPut: [ OrderedCollection new ])
					add: edge.
			 
			list add: edge.
			(done includes: s)
				ifFalse: [ done add: s. workList add: s ] ] ].
	
	^ list
]

{ #category : #testing }
DRSCCP >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph instructions anySatisfy: [ :i | 
		  i isConditionalBranch or: [ self isConstantLattice: (self latticeValue: i) ] ]
]

{ #category : #executing }
DRSCCP >> doApply: cfg [

	self setupWithCFG: cfg.

	self applyPropagation.
]

{ #category : #executing }
DRSCCP >> evaluateInstruction: instruction [

	instruction sccpEvaluateFor: self
]

{ #category : #querying }
DRSCCP >> executedEdgesEntering: aBlock [

	| theEdgesOfThisDestination |
	theEdgesOfThisDestination := edgesByDestination
		at: aBlock ifAbsent: [ #() ].
	^ theEdgesOfThisDestination select: [ :e | e isExecuted ]
]

{ #category : #initialization }
DRSCCP >> initialize [

	super initialize.
	"Identify instructions by their identity to support modifying them"
	latticeValues := IdentityDictionary new.
	ssaWorkList := OrderedCollection new.
	cfgEdgeWorkList := OrderedCollection new.
	
	edgesByDestination := Dictionary new
]

{ #category : #executing }
DRSCCP >> initializeLattice [

	[ cfgEdgeWorkList isEmpty and: [ ssaWorkList isEmpty ] ] whileFalse: [
		self stepCFGEdgeWorklist.
		self stepInstructionWorklist ]
]

{ #category : #initialization }
DRSCCP >> initializeSCCPWithCFG: cfg [

	self initialize.
	instancecfg := cfg.
	
	"This operation marks the edges as unexecuted."
	edges := self buildEdges: cfg.

	"Edges leaving the initial block."
	cfgEdgeWorkList addAll: (edges select: [ :edge | edge source = cfg initialBasicBlock ])
]

{ #category : #executing }
DRSCCP >> isConstantLattice: aLattice [

	| nonConstants |
	nonConstants := { self top. self bottom }.
	^ (nonConstants includes: aLattice) not
]

{ #category : #lattice }
DRSCCP >> latticeValue: aValue [

	"Return the lattice value for the register"
	(aValue isRegister and: [ aValue isVirtualRegister not ])
		ifTrue: [ ^ self bottom ].
	aValue isStore ifTrue: [ ^ self bottom ].
	aValue isInstruction ifFalse: [ ^ aValue ].

	^ latticeValues at: aValue ifAbsent: [ self top ]
]

{ #category : #executing }
DRSCCP >> latticeValueOfPhiFunction: operation [

	| values |

	values := operation operands collect: [ :e | 
		self latticeValue: e ] as: Set.
	values remove: self top ifAbsent: [ "Nothing" ].
	values size = 1 ifTrue: [ ^ values anyOne ].
	^ self bottom
]

{ #category : #lattice }
DRSCCP >> sccpLatticeValueFor: instruction latticeOperand1: operand1 latticeOperand2: operand2 [

	((({ self top. self bottom } includes: operand1) not
		and: [ ({ self top. self bottom } includes: operand2) not ])
		and: [ operand1 isRegister not and: [ operand2 isRegister not ] ])
				ifTrue: [ |op1 op2|
					op1 := operand1.
					op2 := operand2 .
					"If they have different values, choose one to continue the work"
					operand1 class ~= operand2 class
						ifTrue: [ 
							op2 := operand1 class tryParseOperandForSCCP: op2.
							(self isConstantLattice: op2) ifFalse: [ ^ op2 ].
							op1 := operand2 class tryParseOperandForSCCP: op1.
							(self isConstantLattice: op1) ifFalse: [ ^ op1 ]
						].
					"Compute the lattice value and wrap it as a DR value"
					^ (instruction sccpOperateOnLattice: op1 value and: op2 value) asDRValue ].

	(operand1 = self bottom or: [operand2 = self bottom])
		ifTrue: [ ^ self bottom ].

	^ self top
]

{ #category : #lattice }
DRSCCP >> setValueOf: anInstruction at: aValue [

	"Set the new lattice value associated with the instruction.
	Invalidate all its usages so they are recomputed"
	latticeValues at: anInstruction put: aValue.
	ssaWorkList addAll: (self usesOf: anInstruction)
]

{ #category : #executing }
DRSCCP >> setupWithCFG: cfg [

	self initializeSCCPWithCFG: cfg.

	self initializeLattice
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

{ #category : #executing }
DRSCCP >> tryReplaceConditionalJump: i [

	| constantTarget removedTarget |
	"If it is a branch.cond, it has been managed by the previous case"
	i isBranchIfCondition ifTrue: [ 
		| conditionLattice |
		"If it is a branch of the form
		  branch.cond A B
		
		where A cond B is constant, replace it by
		
		  branch.iftrue constant
		"
		conditionLattice := i sccpEvaluateConditionFor: self.
		((self isConstantLattice: conditionLattice) and: [ conditionLattice isJITCompileTimeExpression not]) ifTrue: [ 
			| branchIfTrue |
			branchIfTrue := DRBranchIfTrue operands:
				                { conditionLattice asDRValue }.
			branchIfTrue trueBranch: i trueBranch.
			branchIfTrue falseBranch: i falseBranch.
			i replaceBy: branchIfTrue.
			self tryReplaceConditionalJump: branchIfTrue ].
		^ self ].

	(i isBranchIfTrue and: [ i operand1 isConstant ]) ifFalse: [ ^ self ].

	"If it is a branch of the form
	  branch.IfTrue constant
	
	replace it by simple jump
	"
	i operand1 value
		ifTrue: [ 
			constantTarget := i trueBranch.
			removedTarget := i falseBranch ]
		ifFalse: [ 
			constantTarget := i falseBranch.
			removedTarget := i trueBranch ].

	removedTarget removePredecessor: i basicBlock.
	i replaceBy: (DRJump new target: constantTarget)
]

{ #category : #executing }
DRSCCP >> tryReplaceInstructionByFoldedValue: i [

	| lattice replacement |
	"Only replace if the instruction is not a jump"
	i isJump ifTrue: [ ^ self ].

	lattice := self latticeValue: i.
	(self isConstantLattice: lattice) ifFalse: [ ^ self ].

	replacement := lattice result isNoResult
		               ifTrue: [ lattice ]
		               ifFalse: [
		               DRCopy operands: { lattice asDRValue } result: i result ].

	"We tell the lattice values that the new instruction has the same vlue as the old one"
	latticeValues at: replacement put: (latticeValues at: i).
	i replaceBy: replacement
]

{ #category : #executing }
DRSCCP >> tryReplaceInstructionOperands: i [

	i supportConstantOperand ifFalse: [ ^ self ].

	"Replace the operands of instructions by their propagated values"
	i operands withIndexDo: [ :operand :index | 
		| lattice |
		lattice := self latticeValue: operand.
		(self isConstantLattice: lattice) ifTrue: [ 
			i replaceOperand: (i operands at: index) by: lattice asDRValue ] ]
]

{ #category : #querying }
DRSCCP >> usesOf: anInstruction [

	| ret |
	anInstruction isConditionalBranch ifTrue: [
		ret := OrderedCollection new.
		self value = self top
			ifTrue: [
				ret addAll: anInstruction trueBranch instructions.
				ret addAll: anInstruction falseBranch instructions ]
			ifFalse: [ "We assume self value = self bottom"
				anInstruction condition value
					ifTrue: [ ret addAll: anInstruction trueBranch instructions ]
					ifFalse: [ ret addAll: anInstruction falseBranch instructions ] ].
		^ ret ].
	^ instancecfg instructions select: [ :instruction |
		  instruction operands includes: anInstruction ]
]
