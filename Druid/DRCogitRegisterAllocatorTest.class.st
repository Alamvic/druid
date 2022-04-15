Class {
	#name : #DRCogitRegisterAllocatorTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

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
