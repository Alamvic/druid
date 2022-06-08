Class {
	#name : #DRLinearScanRegisterAllocatorTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testCoallescePhiArguments [

	| cfg basicBlockTrue basicBlockFalse basicBlockJoin firstRegister secondRegister r0 r1 r2 |
	cfg := DRControlFlowGraph new.
	basicBlockTrue := cfg newBasicBlockWith: [ :block |
		r0 := block copy: 2 ].
	basicBlockFalse := cfg newBasicBlockWith: [ :block |
		r1 := block copy: 3 ].
	cfg initialBasicBlock jumpIfTrueTo: basicBlockTrue ifFalseTo: basicBlockFalse.
	basicBlockJoin := cfg newBasicBlock.
	basicBlockTrue jumpTo: basicBlockJoin.
	basicBlockFalse jumpTo: basicBlockJoin.
	
	r2 := basicBlockJoin phiWith: r0 with: r1.
	basicBlockJoin copy: r2.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlockTrue instructions second result equals: firstRegister.
	self assert: basicBlockFalse instructions second result equals: firstRegister.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testInterferingIntervalsWithEnoughRegisters [

	| cfg basicBlock firstRegister secondRegister |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 + r1"
		r2 := block add: r0 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	
	self assert: basicBlock instructions third operand1 result equals: firstRegister.
	self assert: basicBlock instructions third operand2 result equals: secondRegister.
	self assert: ({ firstRegister. secondRegister } includes: basicBlock instructions third result)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetCoversCompletelyThenOverlaps [

	| cfg basicBlock r0 r1 r0Interval r1Interval r2 r3 allocator |

	"
	R0 := 1.
	R1 := 2.
	R2 := R1.
	R3 := R0.
	
	R1 is used before R0 last usage
	=> They cannot occupy the same register
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R1"
		r2 := block copy: r1.
		"R3 := R0"
		r3 := block copy: r0 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	allocator := DRLinearScanRegisterAllocator new
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
	self assert: (r0Interval overlaps: r1Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetCoversCompletelyThenOverlapsCommutative [

	| cfg basicBlock r0 r1 r0Interval r1Interval r2 r3 allocator |

	"
	R0 := 1.
	R1 := 2.
	R2 := R1.
	R3 := R0.
	
	R1 is used before R0 last usage
	=> They cannot occupy the same register
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R1"
		r2 := block copy: r1.
		"R3 := R0"
		r3 := block copy: r0 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	allocator := DRLinearScanRegisterAllocator new
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
		
	self assert: (r1Interval overlaps: r0Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetDoesNotOverlapsIfLastUsageIsDefinition [

	| cfg basicBlock r0 r1 r0Interval r1Interval |

	"
	R0 := 1.
	R1 := R0.
	
	=> We can reuse the register assigned to R0 for R1 if not used below.
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 1"
		r0 := block copy: 1.
		r0 order: 1.
		"R1 := R0"
		r1 := block copy: r0.
		r1 order: 2 ].

	r0Interval := DRLiveSet new
		addRangeFrom: r0 to: r1;
		yourself.
	
	r1Interval := DRLiveSet new
		addRangeFrom: r1 to: r1;
		yourself.
		
	self deny: (r0Interval overlaps: r1Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetDoesNotOverlapsIfLastUsageIsDefinitionCommutative [

	| cfg basicBlock r0 r1 r0Interval r1Interval |
	"
	R0 := 1.
	R1 := R0.
	
	=> We can reuse the register assigned to R0 for R1 if not used below.
	"
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | "R0 := 1"
		              r0 := block copy: 1.
		              r0 order: 1.
		              "R1 := R0"
		              r1 := block copy: r0.
		              r1 order: 2 ].

	r0Interval := DRLiveSet new
		              addRangeFrom: r0 to: r1;
		              yourself.

	r1Interval := DRLiveSet new
		              addRangeFrom: r1 to: r1;
		              yourself.

	self deny: (r1Interval overlaps: r0Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetOverlaps [

	| cfg basicBlock r0 r1 r0Interval r1Interval r2 r3 allocator |

	"
	R0 := 1.
	R1 := 2.
	R2 := R0.
	R3 := R1.
	
	R01 is used before R1 last usage
	=> We cannot reuse the register assigned to R1 for R2.
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R0"
		r2 := block copy: r0.
		"R3 := R1"
		r3 := block copy: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	allocator := DRLinearScanRegisterAllocator new
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
		
	self assert: (r0Interval overlaps: r1Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetOverlapsCommutative [

	| cfg basicBlock r0 r1 r0Interval r1Interval r2 r3 allocator |

	"
	R0 := 1.
	R1 := 2.
	R2 := R0.
	R3 := R1.
	
	R01 is used before R1 last usage
	=> We cannot reuse the register assigned to R1 for R2.
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R0"
		r2 := block copy: r0.
		"R3 := R1"
		r3 := block copy: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	allocator := DRLinearScanRegisterAllocator new
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.		
	self assert: (r1Interval overlaps: r0Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetWithManyIntervalsOverlapsIfOneIntervalOverlaps [

	| cfg r0 r1 r0Interval r1Interval r2 allocator |

	"
	B1:
	 R0 := 1.
	 R1 := 2
	 JumpTo B2
	B2:
	 R2 := R0
	
	Since R0 is used in another block, R0 is alive during B1 and at the beginning of B2.
	Thus it overlaps with R1.
	"

	cfg := DRControlFlowGraph new.
	"R0 := 1"
	r0 := cfg initialBasicBlock copy: 1.
	"R1 := 2"
	r1 := cfg initialBasicBlock copy: 2.
	cfg initialBasicBlock jumpTo: (cfg newBasicBlockWith: [ :block |
		r2 := block copy: r0
	]).

	allocator := DRLinearScanRegisterAllocator new
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
	self assert: (r0Interval overlaps: r1Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetWithManyIntervalsOverlapsIfOtherIntervalOverlaps [

	| cfg r0 r1 r0Interval r1Interval r2 allocator |

	"
	B1:
	 R0 := 1.
	 JumpTo B2
	B2:
	 R1 := 2
	 R2 := R0
	
	Since R0 is used in another block, R0 is alive during B1 and at the beginning of B2.
	Thus it overlaps with R1.
	"

	cfg := DRControlFlowGraph new.
	"R0 := 1"
	r0 := cfg initialBasicBlock copy: 1.
	cfg initialBasicBlock jumpTo: (cfg newBasicBlockWith: [ :block |
		"R1 := 2"
		r1 := block copy: 2.
		r2 := block copy: r0
	]).

	allocator := DRLinearScanRegisterAllocator new
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
	self assert: (r0Interval overlaps: r1Interval)
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testLiveSetsForReturnInstruction [

	| cfg basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		block return ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	DRLinearScanRegisterAllocator new
		registers: { };
		allocateRegistersIn: cfg.
		
	"Kind of smoke test, should not fail"
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testNoSpillHasNoSpillSlots [

	| cfg basicBlock r |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := R0 + 1"
		r1 := block add: r0 to: 1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	r := DRPhysicalRegister name: 'PR1'.
	DRLinearScanRegisterAllocator new
		registers: { r };
		allocateRegistersIn: cfg.
	
	self assert: cfg numberOfSpillSlots equals: 0
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testNonInterferingIntervals [

	| cfg basicBlock r |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := R0 + 1"
		r1 := block add: r0 to: 1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	r := DRPhysicalRegister name: 'PR1'.
	DRLinearScanRegisterAllocator new
		registers: { r };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: r.
	self assert: basicBlock instructions second operand1 result equals: r.
	self assert: basicBlock instructions second result equals: r.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testNonOverlappingVariableInHoleCanReuseHoleRegister [

	| cfg basicBlockTrue basicBlockFalse basicBlockJoin firstRegister r0 r1 r2 rNotOverlapping1 rNotOverlapping2 initialOne |
	cfg := DRControlFlowGraph new.
	initialOne := cfg newBasicBlock.
	basicBlockTrue := cfg newBasicBlockWith: [ :block |
		"will not overlap r1/r0, so it can reuse the same register"
		rNotOverlapping1 := block copy: 70.
		r0 := block copy: 2 ].
	basicBlockFalse := cfg newBasicBlockWith: [ :block |
		"will not overlap r1/r0, so it can reuse the same register"
		rNotOverlapping2 := block copy: 7.
		r1 := block copy: 3 ].
	cfg initialBasicBlock jumpTo: initialOne.
	initialOne jumpIfTrueTo: basicBlockTrue ifFalseTo: basicBlockFalse.
	basicBlockJoin := cfg newBasicBlock.
	basicBlockTrue jumpTo: basicBlockJoin.
	basicBlockFalse jumpTo: basicBlockJoin.
	
	r2 := basicBlockJoin phiWith: r0 with: r1.
	basicBlockJoin copy: r2.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.
	
	self assert: rNotOverlapping1 result equals: r2 result.
	self assert: rNotOverlapping2 result equals: r2 result.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testOverlappingPhiArguments [

	| cfg basicBlockTrue basicBlockJoin firstRegister secondRegister r0 r1 r2 topBasicBlock |
	cfg := DRControlFlowGraph new.

	topBasicBlock := cfg newBasicBlockWith: [ :block |
		r0 := block copy: 2 ].
	basicBlockTrue := cfg newBasicBlockWith: [ :block |
		r1 := block copy: 1 ].
	basicBlockJoin := cfg newBasicBlock.
	
	cfg initialBasicBlock jumpTo: topBasicBlock.
	topBasicBlock
		jumpIf: true asDRValue
		to: basicBlockTrue
		ifFalseTo: basicBlockJoin.
	
	basicBlockTrue jumpTo: basicBlockJoin.
	
	r2 := basicBlockJoin phiWith: r0 with: r1.
	basicBlockJoin copy: r0.
	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: topBasicBlock instructions first result equals: firstRegister.
	self assert: basicBlockTrue instructions first result equals: secondRegister.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testSpillIntroducesLoadBeforeUse [

	| cfg basicBlock firstRegister spillRegister |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 + r1"
		r2 := block add: r0 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	
	"SPR1 is a spill register reserved for spill allocations"
	spillRegister := DRPhysicalRegister name: 'SPR1'.
	DRLinearScanRegisterAllocator new
		registers: { firstRegister };
		spillRegisters: { spillRegister };
		allocateRegistersIn: cfg.
	
	"SPR1 := 2
	Store M0 SPR1
	PR1 := 3
	SPR1 := Load M0
	PR1 := M0 + PR1"
	
	self assert: basicBlock instructions fourth isLoad.
	self assert: basicBlock instructions fourth result equals: spillRegister.
	self assert: basicBlock instructions fifth operand1 result equals: spillRegister.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testSpillIntroducesStore [

	| cfg basicBlock firstRegister spillRegister |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 + r1"
		r2 := block add: r0 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.

	"SPR1 is a spill register reserved for spill allocations"
	spillRegister := DRPhysicalRegister name: 'SPR1'.
	DRLinearScanRegisterAllocator new
		registers: { firstRegister };
		spillRegisters: { spillRegister };
		allocateRegistersIn: cfg.
	
	"SPR1 := 2
	Store M0 SPR1
	PR1 := 3
	SPR1 := Load M0
	PR1 := M0 + PR1"
	
	self assert: basicBlock instructions first result equals: spillRegister.
	self assert: basicBlock instructions second isStore.
	self assert: basicBlock instructions second operand1 equals: spillRegister.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testSpillStoresAndLoadsToSameAddress [

	| cfg basicBlock spillRegister1 spillRegister2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 + r1"
		r2 := block add: r0 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"SPR1 is a spill register reserved for spill allocations"
	spillRegister1 := DRPhysicalRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalRegister name: 'SPR2'.
	DRLinearScanRegisterAllocator new
		spillRegisters: { spillRegister1. spillRegister2 };
		allocateRegistersIn: cfg.
	
	"SPR1 := 2
	Store M0 SPR1
	SPR1 := 3
	Store M1 SPR1
	SPR1 := Load M0
	SPR2 := Load M1
	SPR1 := SPR1 + SPR2"
	
	self
		assert: basicBlock instructions second operand2
		equals: basicBlock instructions fifth operand1.
	self
		assert: basicBlock instructions fourth operand2
		equals: basicBlock instructions sixth operand1
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testSpillTwice [

	| cfg basicBlock spillRegister1 spillRegister2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 + r1"
		r2 := block add: r0 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"SPR1 is a spill register reserved for spill allocations"
	spillRegister1 := DRPhysicalRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalRegister name: 'SPR2'.
	DRLinearScanRegisterAllocator new
		spillRegisters: { spillRegister1. spillRegister2 };
		allocateRegistersIn: cfg.
	
	"SPR1 := 2
	Store M0 SPR1
	SPR1 := 3
	Store M1 SPR1
	SPR1 := Load M0
	SPR2 := Load M1
	SPR1 := SPR1 + SPR2"
	
	self assert: basicBlock instructions first result equals: spillRegister1.
	self assert: basicBlock instructions second isStore.
	self assert: basicBlock instructions third result equals: spillRegister1.
	self assert: basicBlock instructions fourth isStore.
	self assert: basicBlock instructions fifth result equals: spillRegister1.
	self assert: basicBlock instructions sixth result equals: spillRegister2.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testSpillTwiceIncrementsSpillSlotsToTwo [

	| cfg basicBlock spillRegister1 spillRegister2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 + r1"
		r2 := block add: r0 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"SPR1 is a spill register reserved for spill allocations"
	spillRegister1 := DRPhysicalRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalRegister name: 'SPR2'.
	DRLinearScanRegisterAllocator new
		spillRegisters: { spillRegister1. spillRegister2 };
		allocateRegistersIn: cfg.
	
	"Since we have no registers, we need three spill slots: 
	 - two for the ADD operands
	 - one for the ADD result "
	self assert: cfg numberOfSpillSlots equals: 3
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testSpillTwiceUsesDifferentAddresses [

	| cfg basicBlock spillRegister1 spillRegister2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 + r1"
		r2 := block add: r0 to: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"SPR1 is a spill register reserved for spill allocations"
	spillRegister1 := DRPhysicalRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalRegister name: 'SPR2'.
	DRLinearScanRegisterAllocator new
		spillRegisters: { spillRegister1. spillRegister2 };
		allocateRegistersIn: cfg.
	
	"SPR1 := 2
	Store M0 SPR1
	SPR1 := 3
	Store M1 SPR1
	SPR1 := Load M0
	SPR2 := Load M1
	SPR1 := SPR1 + SPR2"
	
	self assert: basicBlock instructions second operand2 stackOffset equals: 1.
	self assert: basicBlock instructions fourth operand2 stackOffset equals: 2.
	self
		deny: basicBlock instructions second operand2
		equals: basicBlock instructions fourth operand2
]
