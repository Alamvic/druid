Class {
	#name : #DRCogitRegisterAllocatorTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testAllocatePhiFunction [

	| cfg firstRegister specialRegister c1 b1 c2 b2 merge |

	specialRegister := DRPhysicalRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block |
		c1 := block copy: 1 ].
	b2 := cfg newBasicBlockWith: [ :block |
		c2 := block copy: 2 ].
	cfg initialBasicBlock jumpIfTrueTo: b1 ifFalseTo: b2.
	merge := cfg newBasicBlock.
	b1 jumpTo: merge.
	b2 jumpTo: merge.
	merge phiWith: c1 with: c2.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.

	"Check that operands and phi function are coallesced"
	self assert: merge instructions first result equals: merge instructions first operand1 result. 
	self assert: merge instructions first result equals: merge instructions first operand2 result. 
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testAllocateRegistersForDivisionIntroducesRemainder [

	| cfg basicBlock firstRegister secondRegister |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 / R1"
		r2 := block divide: r0 by: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	
	"This should allocate registers as:
	R2 := DIVIDE R0 (dividend) R1 (divisor) RN (remainder - unused and thus free after the instruction)
	"
	
	self assert: basicBlock instructions third operand1 result equals: firstRegister.
	self assert: basicBlock instructions third operand2 result equals: secondRegister.
	self assert: ({ firstRegister . secondRegister } includes: basicBlock instructions third operands third result).
	self assert: ({ firstRegister . secondRegister } includes: basicBlock instructions third result).
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testAllocateRegistersForModuloIntroducesQuotient [

	| cfg basicBlock firstRegister secondRegister |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := 3"
		r1 := block copy: 3.
		"R2 := R0 % R1"
		r2 := block modulo: r0 by: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	
	"This should allocate registers as:
	R2 := DIVIDE R0 (dividend) R1 (divisor) RN (quotient - unused and thus free after the instruction)
	"
	
	self assert: basicBlock instructions third operand1 result equals: firstRegister.
	self assert: basicBlock instructions third operand2 result equals: secondRegister.
	self assert: ({ firstRegister . secondRegister } includes: basicBlock instructions third operands third result).
	self assert: ({ firstRegister . secondRegister } includes: basicBlock instructions third result).
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testAllocateTwoSpilledPhiFunctions [

	| cfg specialRegister c1 b1 c2 b2 merge c12 c22 p1 p2 |

	specialRegister := DRPhysicalRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block |
		c1 := block copy: 1.
		c12 := block copy: 12 ].
	b2 := cfg newBasicBlockWith: [ :block |
		c2 := block copy: 2.
		c22 := block copy: 22 ].

	cfg initialBasicBlock jumpIfTrueTo: b1 ifFalseTo: b2.
	merge := cfg newBasicBlock.
	b1 jumpTo: merge.
	b2 jumpTo: merge.

	p1 := merge phiWith: c1 with: c2.
	p2 := merge phiWith: c12 with: c22.
	merge add: p1 to: p2.
	
	DRCogitLinearScanRegisterAllocator new
		registers: { };
		allocateRegistersIn: cfg.

	"Add operands are loaded"
	self assert: merge instructions third isLoad.
	self assert: merge instructions fourth isLoad.
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testCmpSpecialRegisterDoesNotModifyInstruction [

	| cfg basicBlock firstRegister specialRegister |

	"
	R0 := Receiver > 1.
	
	----------
	
	PR1 := Receiver
	PR1 := greater PR1 1
	"

	specialRegister := DRPhysicalRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block greater: specialRegister than: 1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.

	self assert: basicBlock instructions first isCopy. 
	self assert: basicBlock instructions first operand1 equals: specialRegister. 
	self assert: basicBlock instructions second isSetConditionCode.
	self assert: basicBlock instructions second operand1 equals: basicBlock instructions first.
	self assert: basicBlock instructions second result equals: basicBlock instructions first result

]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testCmpSpecialRegisterDoesNotModifyInstructionInversed [

	| cfg basicBlock firstRegister specialRegister |

	"
	R0 := 1 > Receiver
	
	----------------------
	
	PR1 := Receiver
	PR1 := greater 1 PR1

	"

	specialRegister := DRPhysicalRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block greater: 1 than: specialRegister ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.

	self assert: basicBlock instructions first isCopy. 
	self assert: basicBlock instructions first operand1 equals: specialRegister. 
	self assert: basicBlock instructions second isSetConditionCode.
	self assert: basicBlock instructions second operand2 equals: basicBlock instructions first.
	self assert: basicBlock instructions second result equals: basicBlock instructions first result

]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testCoallesceTwoConsecutiveRanges [

	| cfg basicBlock firstRegister secondRegister |

	"From
	
	R0 := 1.
	R1 := 2.
	R2 := R0 + R1.
	R3 := R2 + R1.
	
	To
	
	R0 := 1.
	R1 := 2.
	R0 := R0 + R1.
	R0 := R0 + R1.
	
	After the first three instructions, both registers are free, so both solutions are valid
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 r3 |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R0 + R1"
		r2 := block add: r0 to: r1.
		"R3 := R0"
		r3 := block add: r2 to: r1. ].
	cfg initialBasicBlock jumpTo: basicBlock.

	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.

	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	self assert: basicBlock instructions third result equals: firstRegister.
	self assert: basicBlock instructions fourth result equals: firstRegister.
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testCoallesceTwoOverlappingRanges [

	| cfg basicBlock firstRegister secondRegister thirdRegister |

	"From
	
	T0 := 1.
	T1 := 2.
	T2 := T0 + T1.
	T3 := T0 + T1.
	T4 := T2

	To 2AC conversion

	T0 := 1.
	T1 := 2.
	T5 := T0.
	T2 := T5 + T1.
	T3 := T0 + T1.
	T4 := T2
	
	To
	
	R0 := 1.
	R1 := 2.
	R2 := R0.
	R2 := R2 + R1.
	R0 := R0 + R1.
	R0 := R2
	
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | t0 t1 t2 t3 t4 |
		"T0 := 1"
		t0 := block copy: 1.
		"T1 := 2"
		t1 := block copy: 2.
		"T2 := T0 + T1"
		t2 := block add: t0 to: t1.
		"T3 := T0 + T1"
		t3 := block add: t0 to: t1.
		"T4 := T2"
		t4 := block copy: t2. ].
	cfg initialBasicBlock jumpTo: basicBlock.

	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	thirdRegister := DRPhysicalRegister name: 'PR3'.

	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister. thirdRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	self assert: basicBlock instructions third result equals: thirdRegister.
	self assert: basicBlock instructions fourth result equals: thirdRegister.
	self assert: basicBlock instructions fifth result equals: firstRegister.
	
	"Do not assert anything on the last instruction, since all variables are not live, all registers are available"
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testCoallesceTwoOverlappingRanges2 [

	| cfg basicBlock firstRegister secondRegister thirdRegister |

	"From
	
	T0 := 1.
	T1 := 2.
	T2 := T0 + T1.
	T3 := T0 + T1.
	T4 := T3 + T2.
	T5 := T3.

	To 2AC conversion

	T0 := 1.
	T1 := 2.
	T6 := T0.
	T2 := T6 + T1.
	T3 := T0 + T1.
	T7 := T3.
	T4 := T7 + T2.
	T5 := T3.
	
	To
	
	R0 := 1.
	R1 := 2.
	R2 := R0.
	R2 := R2 + R1.
	R0 := R0 + R1.
	R1 := R0.
	R1 := R1 + R2.
	R1 := R0.	

	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | t0 t1 t2 t3 t4 t5 |
		t0 := block copy: 1.
		t1 := block copy: 2.
		t2 := block add: t0 to: t1.
		t3 := block add: t0 to: t1.
		t4 := block add: t3 to: t2.
		t5 := block copy: t3 ].
	
	cfg initialBasicBlock jumpTo: basicBlock.

	firstRegister := DRPhysicalRegister name: 'PR0'.
	secondRegister := DRPhysicalRegister name: 'PR1'.
	thirdRegister := DRPhysicalRegister name: 'PR2'.

	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister. thirdRegister };
		allocateRegistersIn: cfg.

	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	self assert: basicBlock instructions third result equals: thirdRegister.
	self assert: basicBlock instructions fourth result equals: thirdRegister.
	self assert: basicBlock instructions fifth result equals: firstRegister.
	self assert: basicBlock instructions sixth result equals: secondRegister.
	self assert: basicBlock instructions seventh result equals: secondRegister.

	"Do not assert anything on the last instruction, since all variables are not live, all registers are available"
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testFirstArgumentFarDefinitionNotUsed [

	| cfg basicBlock firstRegister secondRegister |

	"From
	
	R0 := 1.
	R1 := 2.
	R2 := R0 + R1.
	
	To
	
	R0 := 1.
	R1 := 2.
	R0 := R0 + R1.
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R0 + R1"
		r2 := block add: r0 to: r1. ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	self assert: basicBlock instructions third result equals: firstRegister.
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testFirstArgumentIsConstant [

	| cfg basicBlock firstRegister |

	"From
	
	R0 := 1.
	R1 := 1 + R0.
	
	To
	
	R0 := 1.
	R1 := 1 + R0.
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 1 + R0"
		r1 := block add: 1 to: r0.].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: firstRegister.
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testFirstArgumentNotUsed [

	| cfg basicBlock firstRegister secondRegister |

	"From
	
	R0 := 1.
	R1 := 2.
	R2 := R1 + R0.
	
	To
	
	R0 := 1.
	R1 := 2.
	R0 := R1 + R0.
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R1 + R0"
		r2 := block add: r1 to: r0. ].
	cfg initialBasicBlock jumpTo: basicBlock.

	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	self assert: basicBlock instructions third result equals: secondRegister.
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testFirstArgumentSpecialRegisterShouldAddMove [

	| cfg basicBlock firstRegister physicalRegister |

	"From
	
	R1 := 1 + Receiver.
	
	To
	
	R1 := Receiver.
	R1 := 1 + R1.
	"
	physicalRegister := DRPhysicalRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r1 |
		"R1 := 1 + Receiver"
		r1 := block add: 1 to: physicalRegister.].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.
	
	"Copt was inserted R1 := Receiver."
	self assert: basicBlock instructions first operand1 equals: physicalRegister.
	self assert: basicBlock instructions first result equals: firstRegister.
	
	self assert: basicBlock instructions second operand2 result equals: firstRegister.
	self assert: basicBlock instructions second result equals: firstRegister.
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testFirstArgumentUsedCreatesCopy [

	| cfg basicBlock firstRegister secondRegister thirdRegister |

	"From
	
	R0 := 1.
	R1 := 2.
	R2 := R1 + R0.
	R3 := R1.
	
	To (still SSA)
	
	R0 := 1.
	R1 := 2.
	R17 := R1.
	R2 := R17 + R0.
	R3 := R1.
	
	To
	
	R0 := 1.
	R1 := 2.
	R2 := R1.
	R0 := R2 + R0.
	R0/R1/R2 := R1.
	
	After the first four instructions, all three registers are free, so all solutions are valid
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 r3 |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R1 + R0"
		r2 := block add: r1 to: r0.
		"R3 := R1"
		r3 := block copy: r1. ].
	cfg initialBasicBlock jumpTo: basicBlock.


	firstRegister := DRPhysicalRegister name: 'PR0'.
	secondRegister := DRPhysicalRegister name: 'PR1'.
	thirdRegister := DRPhysicalRegister name: 'PR2'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister. thirdRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	
	self assert: basicBlock instructions third isCopy.
	self assert: basicBlock instructions third operand1 result equals: secondRegister.
	self assert: basicBlock instructions third result equals: thirdRegister.
	
	self assert: basicBlock instructions fourth operand1 result equals: thirdRegister.
	self assert: basicBlock instructions fourth result equals: thirdRegister.
	
	self assert: ({ firstRegister . secondRegister. thirdRegister } includes: basicBlock instructions fourth result).
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testSecondArgumentIsConstant [

	| cfg basicBlock firstRegister secondRegister |

	"From
	
	R0 := 1.
	R1 := R0 + 1.
	
	To
	
	R0 := 1.
	R0 := R0 + 1.
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := R0 + 1"
		r1 := block add: r0 to: 1.].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second operand1 result equals: firstRegister.
	self assert: basicBlock instructions second result equals: firstRegister.
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testSecondArgumentUsedButNotFirst [

	| cfg basicBlock firstRegister secondRegister |

	"From
	
	R0 := 1.
	R1 := 2.
	R2 := R0 + R1.
	R3 := R0.
	
	To
	
	R0 := 1.
	R1 := 2.
	R1 := R1 + R0.
	R0/R1 := R0.
	
	After the first three instructions, both registers are free, so both solutions are valid
	"

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 r1 r2 r3 |
		"R0 := 1"
		r0 := block copy: 1.
		"R1 := 2"
		r1 := block copy: 2.
		"R2 := R0 + R1"
		r2 := block add: r1 to: r0.
		"R3 := R0"
		r3 := block copy: r0. ].
	cfg initialBasicBlock jumpTo: basicBlock.

	firstRegister := DRPhysicalRegister name: 'PR1'.
	secondRegister := DRPhysicalRegister name: 'PR2'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister. secondRegister };
		allocateRegistersIn: cfg.
	
	self assert: basicBlock instructions first result equals: firstRegister.
	self assert: basicBlock instructions second result equals: secondRegister.
	self assert: basicBlock instructions third result equals: secondRegister.
	self assert: ({ firstRegister . secondRegister } includes: basicBlock instructions fourth result).
]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testTestSpecialRegisterDoesNotModifyInstruction [

	| cfg basicBlock firstRegister specialRegister |

	"	
	R0 := Receiver Test 1.

	----------
	
	PR1 := Receiver
	PR1 := PR1 Test 1
	"
	
	specialRegister := DRPhysicalRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block anyMask: specialRegister with: 1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.

	self assert: basicBlock instructions first isCopy. 
	self assert: basicBlock instructions first operand1 equals: specialRegister. 
	self assert: basicBlock instructions second isSetConditionCode.
	self assert: basicBlock instructions second operand1 equals: basicBlock instructions first.
	self assert: basicBlock instructions second result equals: basicBlock instructions first result

]

{ #category : #tests }
DRCogitRegisterAllocatorTest >> testTestSpecialRegisterDoesNotModifyInstructionInversed [

	| cfg basicBlock firstRegister specialRegister |

	"	
	R0 := 1 Test Receiver

	----------
	
	PR1 := Receiver
	PR1 := 1 Test PR1
	"
	

	specialRegister := DRPhysicalRegister name: 'Receiver'.
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block anyMask: 1 with: specialRegister ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	firstRegister := DRPhysicalRegister name: 'PR1'.
	DRCogitLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.

	self assert: basicBlock instructions first isCopy. 
	self assert: basicBlock instructions first operand1 equals: specialRegister. 
	self assert: basicBlock instructions second isSetConditionCode.
	self assert: basicBlock instructions second operand2 equals: basicBlock instructions first.
	self assert: basicBlock instructions second result equals: basicBlock instructions first result

]
