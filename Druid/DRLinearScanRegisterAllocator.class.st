Class {
	#name : #DRLinearScanRegisterAllocator,
	#superclass : #Object,
	#instVars : [
		'blocks',
		'liveSets',
		'availableRegisters',
		'spillRegisters',
		'controlFlowGraph'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisterForLiveRegisterInterval: aDRLiveRegisterInterval withActiveIntervals: activeIntervals [
	
	| register intervalToSpill |
	
	availableRegisters ifNotEmpty: [ 
		register := availableRegisters removeFirst.
		^ aDRLiveRegisterInterval location: register ].
	
	"If empty, spill an interval"
	intervalToSpill := activeIntervals anyOne.
	activeIntervals remove: intervalToSpill.
	aDRLiveRegisterInterval location: intervalToSpill location.
	intervalToSpill location: self newStackLocation
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> allocateRegisters [

	| activeIntervals |
	activeIntervals := Set new.
	(liveSets values asSet sorted: [ :a :b | 
		 a start < b start ]) do: [ :currentInterval | 
		activeIntervals copy do: [ :eachInterval | 
			(eachInterval isActiveAt: currentInterval start) ifFalse: [ "not active anymore"
				activeIntervals remove: eachInterval.
				self returnRegister: eachInterval location ] ].

		self
			allocateRegisterForLiveRegisterInterval: currentInterval
			withActiveIntervals: activeIntervals.
		currentInterval isSpilled ifFalse: [ 
			activeIntervals add: currentInterval ] ]
]

{ #category : #api }
DRLinearScanRegisterAllocator >> allocateRegistersIn: aDRControlFlowGraph [ 

	controlFlowGraph := aDRControlFlowGraph.
	self buildLiveSets.
	self allocateRegisters.
	self assignRegisters.
]

{ #category : #accessing }
DRLinearScanRegisterAllocator >> allocatedRegisterFor: aValue [
	
	^ (self liveIntervalOf: aValue) location
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

			allocatedLocation isMemoryAddress ifTrue: [ 
				i addBefore: (DRLoad
						 operands: { 
								 addressSize.
								 allocatedLocation }
						 result: allocatedRegister) ] ].

		"Special case for temporary registers created during register allocation."
		op isVirtualRegister ifTrue: [ 
			op physicalRegister: allocatedRegister ] ].

	usedSpillRegisters do: [ :each | spillRegisters addFirst: each ]
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> buildLiveSets [
	
	"For now do a simple live interval computation.
	This could be further improved with range splitting and other optimizations"
	liveSets := OrderedIdentityDictionary new.
	self linearizeBlocks: controlFlowGraph.
	self computeLiveSets.
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> computeLiveSets [

	self instructionsDo: [ :i | "If the instruction has a result..." 
		self computeLiveSetsForInstruction: i ]
]

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> computeLiveSetsForInstruction: i [

	^ i result ifNotNil: [ 
		  | liveSet |
		  liveSet := DRLiveSet new.
		  liveSet definition: i.
		  i result isVirtualRegister ifTrue: [ 
			  liveSets at: i put: liveSet ].
		
		"Update the live sets of the operands."
		i operands
			  select: [ :operand | operand result isVirtualRegister ]
			  thenDo: [ :operand | self updateLastUsageOf: operand toInstruction: i ].
		  
		"If the instruction is used outside the block,
		consider it is live until the end of the block.
		Otherwise, it's value is dead and do nothing"
		(i users anySatisfy: [ :user | user basicBlock ~= i basicBlock ])
			ifTrue: [ liveSet addRangeFrom: i to: i basicBlock lastInstruction ] ]
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
DRLinearScanRegisterAllocator >> liveIntervalOf: anInstruction [
	
	^ liveSets at: anInstruction
]

{ #category : #allocation }
DRLinearScanRegisterAllocator >> newStackLocation [
	
	^ DRMemoryAddress new
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

{ #category : #'live-analysis' }
DRLinearScanRegisterAllocator >> updateLastUsageOf: instruction toInstruction: usage [

	| rangeStart |
	rangeStart := instruction basicBlock = usage basicBlock
		ifTrue: [ instruction ]
		ifFalse: [ usage basicBlock firstInstruction ].

	(self liveIntervalOf: instruction)
		addRangeFrom: rangeStart to: usage;
		lastUsage: usage
]
