Class {
	#name : #DRLinearScanRegisterAllocatorTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

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
