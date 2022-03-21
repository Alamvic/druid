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
	self assert: basicBlock instructions third result equals: firstRegister.
]

{ #category : #tests }
DRLinearScanRegisterAllocatorTest >> testInterferingIntervalsWithNotEnoughRegisters [

	| cfg basicBlock firstRegister |
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
	DRLinearScanRegisterAllocator new
		registers: { firstRegister };
		allocateRegistersIn: cfg.
	
	"M0 := 2
	PR1 := 3
	PR1 := M0 + PR1"
	
	self assert: basicBlock instructions first result isMemoryAddress.
	self assert: basicBlock instructions second result equals: firstRegister.
	self assert: basicBlock instructions third result equals: firstRegister.
	
	self assert: basicBlock instructions third operand1 result equals: basicBlock instructions first result.
	self assert: basicBlock instructions third operand2 result equals: firstRegister.
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
