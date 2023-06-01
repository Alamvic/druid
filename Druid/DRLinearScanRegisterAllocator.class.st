Class {
	#name : #DRLinearScanRegisterAllocator,
	#superclass : #Object,
	#instVars : [
		'blocks',
		'liveSets',
		'spillRegisters',
		'controlFlowGraph',
		'activeIntervals',
		'inactiveIntervals',
		'unhandledIntervals',
		'spillingStrategy',
		'registerPool'
	],
	#category : #'Druid-LinearScanRegisterAllocation'
}

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisterForInterval: aDRLiveRegisterInterval ifPresent: presentBlock [

	| isFloat inactiveOverlappingRegisters |
	isFloat := aDRLiveRegisterInterval definitions anyOne type
		           isFloatType.

	"Do not consider those that are in inactive intervals so we don't scratch them"
	inactiveOverlappingRegisters := inactiveIntervals
		                                select: [ :e |
		                                e overlaps: aDRLiveRegisterInterval ]
		                                thenCollect: [ :e | e location ].

	isFloat
		ifTrue: [
			registerPool
				takeFloatingPointRegisterNotIn: inactiveOverlappingRegisters
				ifPresent: [ :selectedRegister |
				^ presentBlock value: selectedRegister ] ]
		ifFalse: [
			registerPool
				takeIntegerRegisterNotIn: inactiveOverlappingRegisters
				ifPresent: [ :selectedRegister |
				^ presentBlock value: selectedRegister ] ]
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisterForLiveRegisterInterval: aDRLiveRegisterInterval [

	| intervalToSpill |
	"For now it is active, unless we decide to spill it later"
	activeIntervals add: aDRLiveRegisterInterval.

	self
		allocateRegisterForInterval: aDRLiveRegisterInterval
		ifPresent: [ :selectedRegister |
			^ aDRLiveRegisterInterval location: selectedRegister ].


	"Otherwise, spill an interval and use that register"
	intervalToSpill := self selectIntervalToSpill.
	activeIntervals remove: intervalToSpill.
	aDRLiveRegisterInterval location: intervalToSpill location.
	intervalToSpill location: self newStackLocation
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisters [

	activeIntervals := OrderedCollection new.
	inactiveIntervals := OrderedCollection new.

	unhandledIntervals := (liveSets values asSet sorted: [ :a :b |
		a start < b start and: [ a asString < b definitions asString ] ]) asOrderedCollection.

	[ unhandledIntervals isEmpty ] whileFalse: [ 
		| currentInterval |
		currentInterval := unhandledIntervals removeFirst.
		activeIntervals copy do: [ :eachInterval | 
			(eachInterval isActiveAt: currentInterval start) ifFalse: [ "not active anymore"
				activeIntervals remove: eachInterval.
				self returnRegister: eachInterval location.

				"If the interval is not active but not yet dead, add it to inactive"
				currentInterval start < eachInterval stop ifTrue: [ 
					inactiveIntervals add: eachInterval ] ] ].
		inactiveIntervals copy do: [ :eachInterval | 
			(eachInterval isActiveAt: currentInterval start) ifTrue: [ 
				inactiveIntervals remove: eachInterval.
				activeIntervals add: eachInterval.
				self takeRegister: eachInterval location ] ].

		self allocateRegisterForLiveRegisterInterval: currentInterval ]
]

{ #category : #api }
DRLinearScanRegisterAllocator >> allocateRegistersIn: aDRControlFlowGraph [ 

	self controlFlowGraph: aDRControlFlowGraph.
	self prepareSSA.
	self buildLiveSets.
	self allocateRegisters.
	self assignRegisters.
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> allocatedRegisterFor: aValue [
	
	^ (self liveSetOf: aValue) location
]

{ #category : #steps }
DRLinearScanRegisterAllocator >> assignRegisterToResultInInstruction: i [

	| allocatedLocation allocatedRegister |
	"Rewrite the result of the instruction given as argument, using the location in its pre-computed live interval.
	If the instruction result goes to memory, we need to store it after operating.
	E.g., from
	
	M0 := R1 + R2.
	
	=>
	
	R17 := R1 + R2.
	Store R17 => M0.

	This means that we should reserve at least 1 registers for spilled results.
	Do not add stores for phi functions
	"
	i result isVirtualRegister ifFalse: [ ^ self ].
	i isPhiFunction ifTrue: [
		i result: (self allocatedRegisterFor: i).
		^  self ].

	allocatedLocation := self allocatedRegisterFor: i.
	allocatedRegister := allocatedLocation isMemoryAddress
		                     ifTrue: [ spillRegisters first ]
		                     ifFalse: [ allocatedLocation ].

	i result: allocatedRegister.

	allocatedLocation isMemoryAddress ifTrue: [ 
		i addNext: (DRStore
				 operands: { 
						 8 asDRValue.
						 allocatedRegister.
						 allocatedLocation }
				 result: DRNoRegister new) ]
]

{ #category : #steps }
DRLinearScanRegisterAllocator >> assignRegisters [

	self instructionsDo: [ :i | "Assign first registers to operands.
		This will use spill registers if needed and return them to the pool.
		Then, the result can use them if it is spilled too."
		self assignRegistersToOperandsInInstruction: i.

		"Replace the result by the allocated register if necessary"
		self assignRegisterToResultInInstruction: i ]
]

{ #category : #steps }
DRLinearScanRegisterAllocator >> assignRegistersToOperandsInInstruction: i [

	"Assign first registers to operands.
		This will use spill registers if needed and return them to the pool.
		Then, the result can use them if it is spilled too.
	
	If the instruction operands are in memory, we need to load them before operating.
	E.g., from
	
	R1 := M0 + M1.
	
	=>
	
	R17 := Load M0.
	R18 := Load M1.
	R1 := R17 + R18.
	
	This means that we should reserve at least N registers for spilled operands where N is the maximum number of operands in our instruction set.
	"

	| allocatedLocation allocatedRegister usedSpillRegisters addressSize |
	i isPhiFunction ifTrue: [ ^ self ].

	usedSpillRegisters := OrderedCollection new: 3.
	addressSize := 8 asDRValue.
	i operands do: [ :op | 
		(liveSets includesKey: op) ifTrue: [ 
			allocatedLocation := self allocatedRegisterFor: op.
			allocatedRegister := allocatedLocation isMemoryAddress
				                     ifTrue: [ 
				                     usedSpillRegisters addFirst:
					                     spillRegisters removeFirst ]
				                     ifFalse: [ allocatedLocation ].

			allocatedLocation isMemoryAddress ifTrue: [ | load |
				load := (DRLoad
						 operands: { 
								 addressSize.
								 allocatedLocation }
						 result: allocatedRegister).
				i addBefore: load.
				i replaceOperand: op by: load ].
		
		"Special case for temporary registers created during register allocation."
		op isVirtualRegister ifTrue: [ 
			op physicalRegister: allocatedRegister ] ] ].

	usedSpillRegisters do: [ :each | spillRegisters addFirst: each ]
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> buildLiveSets [
	
	"For now do a simple live interval computation.
	This could be further improved with range splitting and other optimizations"
	liveSets := OrderedIdentityDictionary new.
	self linearizeBlocks: controlFlowGraph.
	self computeLiveSets.
	self coallescePhiIntervals.
]

{ #category : #steps }
DRLinearScanRegisterAllocator >> coallescePhiIntervals [
	
	self instructionsDo: [ :i |
		i isPhiFunction ifTrue: [ | phiSet operandSets allSets mergedSet |
			phiSet := self liveSetOf: i.
			operandSets := i operands collect: [ :op | self liveSetOf: op ].
			allSets := {phiSet}, operandSets.
			
			mergedSet := operandSets
				inject: phiSet
				into: [ :accum :each | accum mergeWith: each ].
			
			liveSets keys do: [ :k |
				(allSets includes: (liveSets at: k))
					ifTrue: [ liveSets at: k put: mergedSet ]
			]
		]
	]
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> computeLiveSets [

	| liveVariablesPerBlock |
	liveVariablesPerBlock := Dictionary new.

	controlFlowGraph postOrderBlocks do: [ :b | 
		| live |
		live := Set new.
		liveVariablesPerBlock at: b put: live.

		"Collect all live variables from successors"
		b successors withIndexDo: [ :s :si | 
			live addAll: (liveVariablesPerBlock at: s ifAbsentPut: [ Set new ]).
			s phiFunctions do: [ :phi | 
				live remove: phi ifAbsent: [  ].
				(phi operandsAtPredecessorsOtherThan: b) do: [ 
					:notIncomingInstruction | 
					(live includes: notIncomingInstruction) ifTrue: [ 
						live remove: notIncomingInstruction ] ] ] ].

		"Check which live values are defined in this block, or defined in predecessors"
		(live sorted: [ :l1 :l2 | l1 name < l2 name ]) do: [ :i | 
			| instructionLiveSet |
			instructionLiveSet := self liveSetOf: i.
			(b instructions includes: i)
				ifTrue: [ 
				instructionLiveSet addRangeFrom: i to: b instructions last ]
				ifFalse: [ 
					instructionLiveSet
						addRangeFrom: b instructions first
						to: b instructions last ] ].

		"Finally, for each instruction we define, augment the live set"
		b instructions reversed do: [ :i | 
			self computeLivenessOfInstruction: i inBlock: b withLiveSet: live ].
		
		
		"For each predecessor that is a backjump, we should extend the life of each variable to be live in the loop"
		b predecessors
			select: [ :p | p endInstruction isBackJumpTo: b ]
			thenDo: [ :p |
				live do: [ :i | | instructionLiveSet |
					instructionLiveSet := self liveSetOf: i.
					instructionLiveSet addRangeFrom: b first to: p endInstruction ] ] ].
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> computeLivenessOfInstruction: i inBlock: b withLiveSet: live [

	i result isVirtualRegister ifTrue: [ 
		live remove: i ifAbsent: [ self initializeLiveSetOfInstruction: i ] ].
	^ i dependencies do: [ :op | 
		  (op result isVirtualRegister and: [ (live includes: op) not ]) 
			  ifTrue: [ 
				  self
					  updateLiveSet: live
					  ofOperand: op
					  ofInstruction: i
					  inBlock: b ] ]
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> controlFlowGraph: aDRControlFlowGraph [

	controlFlowGraph := aDRControlFlowGraph
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> floatRegisters: aCollection [ 

	registerPool floatingPointRegisters: aCollection
]

{ #category : #initialization }
DRLinearScanRegisterAllocator >> initialize [

	super initialize.
	self withFirstSpilling.
	registerPool := DRFixedRegisterPool new
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> initializeLiveSetOfInstruction: anInstruction [

	| liveSetOfOp |
	liveSetOfOp := self liveSetOf: anInstruction.

	liveSetOfOp
		addRangeFrom: anInstruction
		to: anInstruction nextInstruction
]

{ #category : #enumerating }
DRLinearScanRegisterAllocator >> instructionsDo: aFullBlockClosure [ 
	
	blocks do: [ :b |
		b instructionsDo: aFullBlockClosure ]
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> integerRegisters: aCollection [

	registerPool integerRegisters: aCollection
]

{ #category : #'block-linearization' }
DRLinearScanRegisterAllocator >> linearizeBlocks: aDRControlFlowGraph [ 
	
	| order |
	blocks := aDRControlFlowGraph reversePostOrderBlocks.
	
	order := 1.
	self instructionsDo: [ :i | 
		i order: order.
		order := order + 1 ]
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> liveSetOf: anInstruction [

	"Main entry point to get the live set of an instruction"

	^ liveSets at: anInstruction ifAbsentPut: [ DRLiveSet definition: anInstruction ]
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> newStackLocation [
	
	| allocatedSpillSlot |
	allocatedSpillSlot := controlFlowGraph incrementSpillSlots.
	^ DRSpilledMemoryAddress stackOffset: allocatedSpillSlot
]

{ #category : #steps }
DRLinearScanRegisterAllocator >> prepareSSA [
	
	"Prepare the SSA CFG by adding copies above Phi functions.
	Phi function arguments are short lived between 
	 - the end of their predecessors and
	 - the beginning of phi defining block
	
	This copy allows simpler coallescing of the life ranges of the  phi function and its operands.
	"
	controlFlowGraph phiFunctions do: [ :e |
		e operands with: e basicBlock predecessors do: [ :operand :predecessor |
			| copyTarget copy |
			
			"If it is a critical edge, split it and insert the copy in the split block instead"
			copyTarget := predecessor successors size > 1
				ifTrue: [ | splitBlock |
					splitBlock := controlFlowGraph newBasicBlock.
					splitBlock addPredecessor: predecessor.
					predecessor replaceSuccessor: e basicBlock by: splitBlock.
					e basicBlock replacePredecessor: predecessor by: splitBlock.
					splitBlock jumpTo: e basicBlock.
					splitBlock ]
				ifFalse: [ predecessor ].
	
			copy := copyTarget copy: operand.
			e replaceOperandAtPredecessor: copyTarget by: copy
		]
	].

	controlFlowGraph fixBackJumps.
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> registerPool: aRegisterPool [
	
	registerPool := aRegisterPool
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> returnFloatRegister: aPhisicalRegister [

	registerPool returnFloatRegister: aPhisicalRegister
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> returnIntegerRegister: aRegister [

	registerPool returnIntegerRegister: aRegister
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> returnRegister: aPhysicalRegister [

	aPhysicalRegister returnToAllocator: self
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> selectIntervalToSpill [

	^ spillingStrategy value
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> spillRegisters: aCollection [
	
	spillRegisters := aCollection asOrderedCollection
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> takeFloatRegister: aDRPhysicalRegister [
	
	registerPool takeFloatRegister: aDRPhysicalRegister
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> takeIntegerRegister: aRegister [

	registerPool takeIntegerRegister: aRegister
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> takeRegister: aDRPhysicalRegister [ 
	
	aDRPhysicalRegister takeFromAllocator: self
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> updateLiveSet: liveSet ofOperand: op ofInstruction: i inBlock: b [

	| liveSetOfOp |
	liveSet add: op.
	liveSetOfOp := self liveSetOf: op.
	(b instructions includes: op)
		ifTrue: [ liveSetOfOp addRangeFrom: op to: i ]
		ifFalse: [ liveSetOfOp addRangeFrom: b instructions first to: i ]
]

{ #category : #'spilling strategies' }
DRLinearScanRegisterAllocator >> withFirstSpilling [

	spillingStrategy := [ activeIntervals first ].
]

{ #category : #'spilling strategies' }
DRLinearScanRegisterAllocator >> withFurthestIntervalSpilling [

	spillingStrategy := [ 
		activeIntervals detectMax: [ :e | e stop ] ].
]

{ #category : #'spilling strategies' }
DRLinearScanRegisterAllocator >> withLastSpilling [

	spillingStrategy := [ activeIntervals last ].
]

{ #category : #'spilling strategies' }
DRLinearScanRegisterAllocator >> withLeastUsedInterval [

	spillingStrategy := [ activeIntervals detectMin: [ :e | e numberOfUsers ] ]
]

{ #category : #'spilling strategies' }
DRLinearScanRegisterAllocator >> withRandomSpilling [

	spillingStrategy := [ activeIntervals atRandom ].
]
