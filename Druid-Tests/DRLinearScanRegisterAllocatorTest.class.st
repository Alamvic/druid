Class {
	#name : #DRLinearScanRegisterAllocatorTest,
	#superclass : #TestCase,
	#instVars : [
		'registerAllocator'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> setUp [

	registerAllocator := DRCogitLinearScanRegisterAllocator new
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testAllocateFloatRegister [

	| cfg basicBlock r fr |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		"R0 := 2"
		r0 := block copy: 2.
		"FR1 := IntToFloat R0"
		r1 := block intToFloat: r0 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	r := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	fr := DRPhysicalFloatingPointRegister name: 'FPR1'.
	registerAllocator 
		integerRegisters: { r };
		floatRegisters: { fr };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: r.
	self assert: basicBlock instructions second operand1 result equals: r.
	self assert: basicBlock instructions second result equals: fr.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testAllocateFloatRegistersAfterOperate [

	| cfg basicBlock r fr fr2 r0 r1 r3 r4 r5 r6 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | 
		r0 := block copy: 2.
		r1 := block intToFloat: r0.
		r3 := block copy: 3.
		r4 := block intToFloat: r3.
		r5 := block add: r1 to: r4. "FPR1 + FPR2"
		r6 := block copy: r5 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	r := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	fr := DRPhysicalFloatingPointRegister name: 'FPR1'.
	fr2 := DRPhysicalFloatingPointRegister name: 'FPR2'.
	registerAllocator 
		integerRegisters: { r };
		floatRegisters: { fr . fr2 };
		allocateRegistersIn: cfg.
			
	self assert: r5 result name equals: r5 operand1 result name

]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testAllocateFloatRegistersAfterOperateReversed [

	| cfg basicBlock r fr fr2 r0 r1 r3 r4 r5 r6 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | 
		r0 := block copy: 2.
		r1 := block intToFloat: r0.
		r3 := block copy: 3.
		r4 := block intToFloat: r3.
		r5 := block add: r4 to: r1. "FPR2 + FPR1"
		r6 := block copy: r5 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	r := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	fr := DRPhysicalFloatingPointRegister name: 'FPR1'.
	fr2 := DRPhysicalFloatingPointRegister name: 'FPR2'.
	registerAllocator 
		integerRegisters: { r };
		floatRegisters: { fr . fr2 };
		allocateRegistersIn: cfg.
			
	self assert: r5 result name equals: r5 operand1 result name

]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testAllocateIntegerRegistersAfterOperate [

	| cfg basicBlock pr1 pr2 r0 r1 r2 r3 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | 
		r0 := block copy: 2.
		r1 := block copy: 3.
		r2 := block add: r0 to: r1. "PR1 + PR2"
		r3 := block copy: r2 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	pr1 := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	pr2 := DRPhysicalGeneralPurposeRegister name: 'PR2'.
	registerAllocator 
		integerRegisters: { pr1 . pr2 };
		allocateRegistersIn: cfg.
		
	self assert: r2 result name equals: r2 operand1 result name

]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testAllocateIntegerRegistersAfterOperateReversed [

	| cfg basicBlock pr1 pr2 r0 r1 r2 r3 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | 
		r0 := block copy: 2.
		r1 := block copy: 3.
		r2 := block add: r1 to: r0. "PR2 + PR1"
		r3 := block copy: r2 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	pr1 := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	pr2 := DRPhysicalGeneralPurposeRegister name: 'PR2'.
	registerAllocator 
		integerRegisters: { pr1 . pr2 };
		allocateRegistersIn: cfg.
		
	self assert: r2 result name equals: r2 operand1 result name

]

{ #category : #'tests-ssa preparation' }
DRLinearScanRegisterAllocatorTest >> testBreakPhiDuringSSAPreparation [

	| cfg basicBlockTrue basicBlockJoin r0 r1 r2 topBasicBlock |
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
	
	registerAllocator
		controlFlowGraph: cfg;
		prepareSSA.
	
	self assert: (basicBlockJoin predecessors first instructions atLast: 2) isCopy.
	self assert: (basicBlockJoin predecessors first instructions atLast: 2) operand1 equals: r0.
	
	self assert: (basicBlockJoin predecessors second instructions atLast: 2) isCopy.
	self assert: (basicBlockJoin predecessors second instructions atLast: 2) operand1 equals: r1.
]

{ #category : #'tests-ssa preparation' }
DRLinearScanRegisterAllocatorTest >> testBreakPhiDuringSSAPreparationWithStaging [

	| cfg basicBlockTrue basicBlockJoin r0 r1 r2 topBasicBlock firstGeneratedCopy secondGeneratedCopy |
	cfg := DRControlFlowGraph new.

	topBasicBlock := cfg newBasicBlockWith: [ :block |
		                 r0 := (block copy: 2) markAsStaged ].
	basicBlockTrue := cfg newBasicBlockWith: [ :block |
		                  r1 := (block copy: 1) markAsStaged ].
	basicBlockJoin := cfg newBasicBlock.

	cfg initialBasicBlock jumpTo: topBasicBlock.
	topBasicBlock
		jumpIf: true asDRValue
		to: basicBlockTrue
		ifFalseTo: basicBlockJoin.

	basicBlockTrue jumpTo: basicBlockJoin.

	r2 := (basicBlockJoin phiWith: r0 with: r1) markAsStaged.
	basicBlockJoin copy: r0.

	registerAllocator
		controlFlowGraph: cfg;
		prepareSSA.

	"If phi and operands are staged, we should also stage the copies"
	firstGeneratedCopy := basicBlockJoin predecessors first instructions
		                      atLast: 2.
	self assert: firstGeneratedCopy isCopy.
	self assert: firstGeneratedCopy isStaged.
	self assert: firstGeneratedCopy operand1 equals: r0.

	secondGeneratedCopy := basicBlockJoin predecessors second
		                       instructions atLast: 2.
	self assert: secondGeneratedCopy isCopy.
	self assert: secondGeneratedCopy isStaged.
	self assert: secondGeneratedCopy operand1 equals: r1.
	
	self assert: firstGeneratedCopy result equals: secondGeneratedCopy result
]

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
	
	firstRegister := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	secondRegister := DRPhysicalGeneralPurposeRegister name: 'PR2'.
	registerAllocator 
		integerRegisters: { firstRegister. secondRegister };
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
	
	firstRegister := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	secondRegister := DRPhysicalGeneralPurposeRegister name: 'PR2'.
	registerAllocator 
		integerRegisters: { firstRegister. secondRegister };
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

	allocator := registerAllocator
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

	allocator := registerAllocator
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
		
	self assert: (r1Interval overlaps: r0Interval)
]

{ #category : #'tests - live set overlap' }
DRLinearScanRegisterAllocatorTest >> testLiveSetCoversUntilLoad [

	| cfg basicBlock r0 r1 r0Interval r1Interval r3 allocator |

	"
	R0 := 1.
	R1 := 2.
	R2 := LOAD R0.
	
	R0 is used after R1
	=> They cannot occupy the same register
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := LOAD R0"
		r3 := block load: r0 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	allocator := registerAllocator
		controlFlowGraph: cfg;
		buildLiveSets;
		yourself.
	
	r0Interval := allocator liveSetOf: r0.
	r1Interval := allocator liveSetOf: r1.
	self assert: (r0Interval overlaps: r1Interval)
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

	allocator := registerAllocator
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

	allocator := registerAllocator
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

	allocator := registerAllocator
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

	allocator := registerAllocator
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
	
	registerAllocator
		integerRegisters: { };
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
	
	r := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	registerAllocator
		integerRegisters: { r };
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
	
	r := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	registerAllocator
		integerRegisters: { r };
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
	
	firstRegister := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	registerAllocator
		integerRegisters: { firstRegister };
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
	firstRegister := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	secondRegister := DRPhysicalGeneralPurposeRegister name: 'PR2'.
	registerAllocator
		integerRegisters: { firstRegister. secondRegister };
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
	
	firstRegister := DRPhysicalGeneralPurposeRegister name: 'PR1'.
	
	"SPR1 is a spill register reserved for spill allocations"
	spillRegister := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	registerAllocator
		integerRegisters: { firstRegister };
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
DRLinearScanRegisterAllocatorTest >> testSpillIntroducesLoadBeforeUseWithMemoryAccess [

	"From
	
	T0 := 1.
	T1 := 2.
	T2 := LOAD T0
	T3 := T0
	
	To

	SPR1 := 1.
	Store M0 SPR1
	PR1 := 2.
	SPR1 := Load M0
	T2 := LOAD SPR1
	SPR1 := Load M0
	T3 := SPR1
	"

	| cfg t0 t1 t2 t3 basicBlock |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"T0 := 1"
		t0 := block copy: 1.
		"T1 := 2"
		t1 := block copy: 2.
		"T2 := LOAD T0"
		t2 := block load: t0.
		"T3 := T0"
		t3 := block copy: t0. ].
	cfg initialBasicBlock jumpTo: basicBlock.

	DRCogitLinearScanRegisterAllocator new
		integerRegisters: { DRPhysicalGeneralPurposeRegister name: 'PR1' };
		allocateRegistersIn: cfg.

	"Store first copy"	
	self assert: t0 nextInstruction isStore.
	self assert: t0 nextInstruction operand1 equals: t0 result.
	self assert: t0 nextInstruction address stackOffset equals: 1.

	"Load on each use"
	self assert: t2 address expression result equals: t2 previousInstruction result.
	self assert: t2 previousInstruction isLoad.
	self assert: t2 previousInstruction address stackOffset equals: 1.

	self assert: t3 operand result equals: t3 previousInstruction result.
	self assert: t3 previousInstruction isLoad.
	self assert: t3 previousInstruction address stackOffset equals: 1.
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
	
	firstRegister := DRPhysicalGeneralPurposeRegister name: 'PR1'.

	"SPR1 is a spill register reserved for spill allocations"
	spillRegister := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	registerAllocator
		integerRegisters: { firstRegister };
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
	spillRegister1 := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalGeneralPurposeRegister name: 'SPR2'.
	registerAllocator
		integerRegisters: { };
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
	spillRegister1 := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalGeneralPurposeRegister name: 'SPR2'.
	registerAllocator
		integerRegisters: { };
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
	spillRegister1 := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalGeneralPurposeRegister name: 'SPR2'.
	registerAllocator
		integerRegisters: { };
		spillRegisters: { spillRegister1. spillRegister2 };
		allocateRegistersIn: cfg.
	
	self assert: cfg numberOfSpillSlots equals: 2
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
	spillRegister1 := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	spillRegister2 := DRPhysicalGeneralPurposeRegister name: 'SPR2'.
	registerAllocator
		integerRegisters: { };
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
