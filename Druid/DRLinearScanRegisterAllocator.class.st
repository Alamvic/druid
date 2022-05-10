Class {
	#name : #DRLinearScanRegisterAllocator,
	#superclass : #Object,
	#instVars : [
		'blocks',
		'liveSets',
		'availableRegisters',
		'spillRegisters',
		'controlFlowGraph',
		'activeIntervals',
		'inactiveIntervals',
		'unhandledIntervals'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisterForLiveRegisterInterval: aDRLiveRegisterInterval [
	
	| intervalToSpill currentRegisters |
	currentRegisters := availableRegisters copy.
	inactiveIntervals
		select: [ :e | e overlaps: aDRLiveRegisterInterval ]
		thenDo: [ :e | currentRegisters remove: e location ifAbsent: [ "ignore if it is in use" ] ].
	unhandledIntervals
		select: [ :e | e location notNil and: [ e overlaps: aDRLiveRegisterInterval ] ]
		thenDo: [ :e | e halt ].
	
	currentRegisters ifNotEmpty: [ | selectedRegister |
		selectedRegister := currentRegisters first.
		self takeRegister: selectedRegister.
		^ aDRLiveRegisterInterval location: currentRegisters first ].

	"If empty, spill an interval"
	intervalToSpill := activeIntervals
		ifEmpty: [ aDRLiveRegisterInterval ]
		ifNotEmpty: [ activeIntervals remove: activeIntervals anyOne ].
	aDRLiveRegisterInterval location: intervalToSpill location.
	intervalToSpill location: self newStackLocation
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisters [

	activeIntervals := Set new.
	inactiveIntervals := Set new.

	unhandledIntervals := (liveSets values asSet sorted: [ :a :b | 
		                       a start < b start ]) asOrderedCollection.

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

		self allocateRegisterForLiveRegisterInterval: currentInterval.
		currentInterval isSpilled ifFalse: [ 
			activeIntervals add: currentInterval ] ]
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

{ #category : #assigning }
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
	"
	
	i result ifNil: [ ^ self ].
	i result isVirtualRegister ifFalse: [ ^ self ].

	allocatedLocation := self allocatedRegisterFor: i.
	allocatedRegister := allocatedLocation isMemoryAddress
		ifTrue: [ spillRegisters first ]
		ifFalse: [ allocatedLocation ].
	
	i result: allocatedRegister.

	allocatedLocation isMemoryAddress ifTrue: [ 
		i addNext: (DRStore
			 operands: { allocatedRegister }
			 result: allocatedLocation)]
]

{ #category : #assigning }
DRLinearScanRegisterAllocator >> assignRegisters [

	self instructionsDo: [ :i | "Assign first registers to operands.
		This will use spill registers if needed and return them to the pool.
		Then, the result can use them if it is spilled too."
		self assignRegistersToOperandsInInstruction: i.

		"Replace the result by the allocated register if necessary"
		self assignRegisterToResultInInstruction: i ]
]

{ #category : #assigning }
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

{ #category : #coallescing }
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

	controlFlowGraph postOrderBlocks do: [ :b | | live |
		live := Set new.
		liveVariablesPerBlock at: b put: live.
		b successors withIndexDo: [ :s :si |
			live addAll: (liveVariablesPerBlock at: s ifAbsentPut: [ Set new ]).
			s phiFunctions do: [ :phi |
				live remove: phi ifAbsent: [ ].
				(phi operandsAtPredecessorsOtherThan: b) do: [ :notIncomingInstruction |
					(live includes: notIncomingInstruction)
						ifTrue: [ live remove: notIncomingInstruction ] ].
			] ].
		live do: [ :i | | instructionLiveSet |
			instructionLiveSet := self liveSetOf: i.
			(b instructions includes: i)
				ifTrue: [ instructionLiveSet addRangeFrom: i to: b instructions last ]
				ifFalse: [ instructionLiveSet addRangeFrom: b instructions first to: b instructions last ]	
		].
		b instructions reversed
			select: [ :i | i result isVirtualRegister ]
			thenDo: [ :i |
				live
					remove: i
					ifAbsent: [ self initializeLiveSetOfInstruction: i ].
				i operands do: [ :op |
					(op result isVirtualRegister and: [(live includes: op) not]) ifTrue: [
						self updateLiveSet: live ofOperand: op ofInstruction: i inBlock: b
					]
				]
			]
		].
]

{ #category : #api }
DRLinearScanRegisterAllocator >> controlFlowGraph: aDRControlFlowGraph [

	controlFlowGraph := aDRControlFlowGraph
]

{ #category : #initialization }
DRLinearScanRegisterAllocator >> initialize [

	super initialize.
	availableRegisters := #()
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
	
	^ liveSets at: anInstruction ifAbsentPut: [ DRLiveSet new ]
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> newStackLocation [
	
	| allocatedSpillSlot |
	allocatedSpillSlot := controlFlowGraph incrementSpillSlots.
	^ DRSpilledMemoryAddress stackOffset: allocatedSpillSlot
]

{ #category : #'as yet unclassified' }
DRLinearScanRegisterAllocator >> prepareSSA [
	
	"Prepare the SSA CFG by adding copies above Phi functions.
	Phi function arguments are short lived between 
	 - the end of their predecessors and
	 - the beginning of phi defining block
	
	This copy allows simpler coallescing of the life ranges of the  phi function and its operands.
	"
	
	controlFlowGraph phiFunctions do: [ :e |
		e operands with: e basicBlock predecessors do: [ :operand :predecessor |
			| copy |
			copy := predecessor copy: operand.
			e replaceOperand: operand by: copy
		]
	]
]

{ #category : #'block-linearization' }
DRLinearScanRegisterAllocator >> registers: aCollection [

	availableRegisters := aCollection asOrderedCollection
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> returnRegister: aPhisicalRegister [

	availableRegisters addFirst: aPhisicalRegister
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> spillRegisters: aCollection [
	
	spillRegisters := aCollection asOrderedCollection
]

{ #category : #registers }
DRLinearScanRegisterAllocator >> takeRegister: aDRPhysicalRegister [ 
	
	availableRegisters remove: aDRPhysicalRegister
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

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> updateLiveSetOfOperand: op ofInstruction: i inBlock: b [

	| liveSetOfOp |
	liveSetOfOp := self liveSetOf: op.
	(b instructions includes: op)
		ifTrue: [ liveSetOfOp addRangeFrom: op to: i ]
		ifFalse: [ liveSetOfOp addRangeFrom: b instructions first to: i ]
]
