Class {
	#name : #DRLinearScanRegisterAllocatorTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

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
		definition: r0;
		lastUsage: r1;
		yourself.
	
	r1Interval := DRLiveSet new
		definition: r1;
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
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 1"
		r0 := block copy: 1.
		r0 order: 1.
		"R1 := R0"
		r1 := block copy: r0.
		r1 order: 2 ].

	r0Interval := DRLiveSet new
		definition: r0;
		lastUsage: r1;
		yourself.
	
	r1Interval := DRLiveSet new
		definition: r1;
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

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testNoOverlapInConditionalBranches [

	| cfg r0 r1 r0Interval r1Interval r2 allocator b2 b3 b4 |

	"
	B1:
	 JumpIf x B2 else B3
	B2:
	 R0 := 0
	 Jump B4
	B3:
	 R1 := 1
	 Jump B4
	B4:
	 R2 := phi(R0, R1)
	
	Since B2 and B3 are exclusive, they cannot overlap.
	"

	cfg := DRControlFlowGraph new.
	b2 := cfg newBasicBlockWith: [ :b | r0 := b copy: 0 ].
	b3 := cfg newBasicBlockWith: [ :b | r1 := b copy: 1 ].
	b4 := cfg newBasicBlock.
	cfg initialBasicBlock jumpIfTrueTo: b2 ifFalseTo: b3.
	b2 jumpTo: b4.
	b3 jumpTo: b4.
	b4 phiWith: r0 with: r1.

	allocator := DRLinearScanRegisterAllocator new
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
	self deny: (r0Interval overlaps: r1Interval)
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
