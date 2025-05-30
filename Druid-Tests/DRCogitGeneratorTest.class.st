Class {
	#name : 'DRCogitGeneratorTest',
	#superclass : 'DRIRTest',
	#instVars : [
		'generator'
	],
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'helpers' }
DRCogitGeneratorTest >> generateCogitForInstruction: aDRInstruction [

	generator buildStatementsForInstruction: aDRInstruction
]

{ #category : 'helpers' }
DRCogitGeneratorTest >> setUp [

	super setUp.
	generator := DRCogitCodeGenerator new
]

{ #category : 'accessing' }
DRCogitGeneratorTest >> statements [

	^ generator generatorMethodBuilder statements
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testConstantsBiggerThan5000AreInHexa [

	| cfg basicBlock r0 move2ACASTInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 2"
		r0 := block copy: 8000.].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"Transform 3AC to 2AC =>
	R0 := 16r1F40.
	"
	self generateCogitForInstruction: r0.
	
	move2ACASTInstruction := self statements first.
	self assert: move2ACASTInstruction selector equals: #MoveCq:R:.
	self assert: move2ACASTInstruction arguments first value equals: 8000.
	self assert: move2ACASTInstruction arguments first sourceText equals: '16r1F40'.
	self assert: move2ACASTInstruction arguments second name equals: 't0'
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateAndBetweenRegistersAndNoResult [

	| cfg basicBlock r0 r1 orASTInstruction r2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 2"
		r0 := block copy: 2.
		r1 := block copy: 3.
		"R2 := R0 AND R1"
		r2 := block and: r0 with: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	"Guarantee we are in two address code, and we need no result value"	
	r2 result: DRNoRegister new.
	
	self generateCogitForInstruction: r2.	
	orASTInstruction := self statements first.

	"Should generate: cogit AndR: R1 R: R0.
	In Druid 2AC, the destination is bound to the first operand.
	In Cogit, to the second operand"
	self assert: orASTInstruction selector equals: #AndR:R:.
	self assert: orASTInstruction arguments first name equals: 't1'.
	self assert: orASTInstruction arguments second name equals: 't0'
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateBitAndEqualsZero [

	| cfg b1 b2 basicBlock r1 instruction |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block loadFramePointer.
		r1 := block addInstruction: ((block instructionFactory jumpIf: true trueTarget: b1 falseTarget: b2)
			operands: {DRNoTestMask new. r0. (17 asDRValue)};
			yourself).
	].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	self generateCogitForInstruction: r1.

	instruction := self statements first.
	self assert: instruction selector equals: #TstCq:R:.
	self assert: instruction arguments first value equals: 17.
	self assert: instruction arguments second name equals: 't0'.
	
	self assert: self statements second value selector equals: #JumpZero:.
	self assert: self statements third value selector equals: #JumpNonZero:.

]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateBitAndNotEqualsZero [

	| cfg b1 b2 basicBlock r1 instruction |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block loadFramePointer.
		r1 := block addInstruction: ((block instructionFactory jumpIf: true trueTarget: b1 falseTarget: b2)
			operands: {DRTestMask new. r0. (17 asDRValue)};
			yourself).
	].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	self generateCogitForInstruction: r1.

	instruction := self statements first.
	self assert: instruction selector equals: #TstCq:R:.
	self assert: instruction arguments first value equals: 17.
	self assert: instruction arguments second name equals: 't0'.
	
	self assert: self statements second value selector equals: #JumpNonZero:.
	self assert: self statements third value selector equals: #JumpZero:.

]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateDivision [

	| cfg basicBlock r1 divideASTInstruction r2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		"R0 := 2"
		r0 := block copy: 2.
		r1 := block copy: 1.
		"R2 := R0 / R1"
		r2 := block divide: r0 by: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
		
	"The Cogit specific register allocator should allocate a free register for the remainder.
	Simulate this in the test for now"
	r2 addOperand: cfg instructionFactory allocateTemporaryRegister.
	
	"Cogit division is not in 2AC but in 4AC.
	Inserts REM Destination implicitly yet unused, and puts all operands in registers.
	
	R0 := 2. (Dividend)
	R1 := 1. (Divisor)
	DIVIDE R1 R0 R2 (quotient) R3 (remainder)
	"

	self generateCogitForInstruction: r2.	
	divideASTInstruction := self statements first.
	self assert: divideASTInstruction selector equals: #DivR:R:Quo:Rem:.
	self assert: divideASTInstruction arguments first name equals: 't1'.
	self assert: divideASTInstruction arguments second name equals: 't0'.
	self assert: divideASTInstruction arguments third name equals: 't2'.
	self assert: divideASTInstruction arguments fourth name equals: 't3'.
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateDruidComment [

	| cfg comment builder |
	cfg := DRPrimitiveControlFlowGraph new.
	generator generateCodeForCFG: cfg.
	builder := generator generatorMethodBuilder.
	builder selector: 'gen_primitiveTest'.
	comment := builder methodNode comments first.
	self assert: comment contents equals: 'AutoGenerated by Druid'
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateFalseAsZero [

	| cfg basicBlock r0 compare |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		r0 := block copy: false asDRValue. ].
	cfg initialBasicBlock jumpTo: basicBlock.

	self generateCogitForInstruction: r0.	
	compare := self statements first.

	self assert: compare selector equals: #MoveCq:R:.
	self assert: compare arguments first value equals: 0
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateJmpIfAnd [

	| cfg b1 b2 basicBlock r1 instruction |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block loadFramePointer.
		r1 := block addInstruction: ((block instructionFactory jumpIf: true trueTarget: b1 falseTarget: b2)
			operands: {DRTestAnd new. r0. (17 asDRValue)};
			yourself).
	].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	self generateCogitForInstruction: r1.

	instruction := self statements first.
	self assert: instruction selector equals: #AndCq:R:.
	self assert: instruction arguments first value equals: 17.
	self assert: instruction arguments second name equals: 't0'.
	
	self assert: self statements second value selector equals: #JumpNonZero:.
	self assert: self statements third value selector equals: #JumpZero:.

]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateJmpIfNand [

	| cfg b1 b2 basicBlock r1 instruction |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 1 at: 888 ].
	b2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		r0 := block loadFramePointer.
		r1 := block addInstruction: ((block instructionFactory jumpIf: true trueTarget: b1 falseTarget: b2)
			operands: {DRNoTestAnd new. r0. (17 asDRValue)};
			yourself).
	].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	self generateCogitForInstruction: r1.

	instruction := self statements first.
	self assert: instruction selector equals: #AndCq:R:.
	self assert: instruction arguments first value equals: 17.
	self assert: instruction arguments second name equals: 't0'.
	
	self assert: self statements second value selector equals: #JumpZero:.
	self assert: self statements third value selector equals: #JumpNonZero:.

]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateLoadSpilledMemoryAddress [

	| cfg basicBlock loadInstruction r0 |

	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := LOAD STACK-1"
		r0 := block load: (DRSpilledMemoryAddress stackOffset: 1) ].
	cfg initialBasicBlock jumpTo: basicBlock.
			
	"Should generate
	
	self MoveM64: 8 r: SPReg R: R0
	"

	self generateCogitForInstruction: r0.	
	loadInstruction := self statements first.
	self assert: loadInstruction selector equals: #MoveMw:r:R:.
	self assert: loadInstruction arguments first value equals: 0.
	self assert: loadInstruction arguments second name equals: 'SPReg'.
	self assert: loadInstruction arguments third name equals: 't0'
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateMethodWithSpillingAllocatesStackSpace [

	| cfg basicBlock spillRegister1 stackAllocationInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | block copy: 2 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	spillRegister1 := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	DRCogitLinearScanRegisterAllocator new
		integerRegisters: #(  );
		spillRegisters: { spillRegister1 };
		allocateRegistersIn: cfg.
	generator generatePreambleForCFG: cfg.

	stackAllocationInstruction := self statements first.
	self assert: stackAllocationInstruction selector equals: #SubCq:R:.
	self
		assert: stackAllocationInstruction arguments second name
		equals: 'SPReg'.
	self
		assert: stackAllocationInstruction arguments first value
		equals: cfg numberOfSpillSlots * 8 "bytes"
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateMethodWithSpillingDeallocatesStackSpace [

	| cfg basicBlock spillRegister1 stackAllocationInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | block copy: 2 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	spillRegister1 := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	DRCogitLinearScanRegisterAllocator new
		integerRegisters: #(  );
		spillRegisters: { spillRegister1 };
		allocateRegistersIn: cfg.
	generator generatePostambleForCFG: cfg.

	stackAllocationInstruction := self statements last.
	self assert: stackAllocationInstruction selector equals: #AddCq:R:.
	self
		assert: stackAllocationInstruction arguments second name
		equals: 'SPReg'.
	self
		assert: stackAllocationInstruction arguments first value
		equals: cfg numberOfSpillSlots * 8 "bytes"
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateModulo [

	| cfg basicBlock r1 divideASTInstruction r2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		"R0 := 2"
		r0 := block copy: 2.
		r1 := block copy: 1.
		"R2 := R0 % R1"
		r2 := block modulo: r0 by: r1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
		
	"The Cogit specific register allocator should allocate a free register for the quotient.
	Simulate this in the test for now"
	r2 addOperand: cfg instructionFactory allocateTemporaryRegister.
	
	"Cogit division is not in 2AC but in 4AC.
	Inserts Quotient Destination implicitly yet unused, and puts all operands in registers.
	
	R0 := 2. (Dividend)
	R1 := 1. (Divisor)
	DIVIDE R1 R0 R3 (quotient) R2 (remainder)
	"

	self generateCogitForInstruction: r2.	
	divideASTInstruction := self statements first.
	self assert: divideASTInstruction selector equals: #DivR:R:Quo:Rem:.
	self assert: divideASTInstruction arguments first name equals: 't1'.
	self assert: divideASTInstruction arguments second name equals: 't0'.
	self assert: divideASTInstruction arguments third name equals: 't3'.
	self assert: divideASTInstruction arguments fourth name equals: 't2'.
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateNilAsZero [

	| cfg basicBlock r0 r1 compare |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"Compare nil 0"
		r0 := block copy: 0.
		r1 := block equals: nil to: r0 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	DRCogitOperandSorter new applyTo: cfg.	
	
	self generateCogitForInstruction: r1 previousInstruction.
	compare := self statements first.
	self assert: compare selector equals: #MoveCq:R:.
	self assert: compare arguments first value equals: 0
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateOrConstant [

	| cfg basicBlock r0 r1 orASTInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := R0 OR false"
		r1 := block or: r0 with: false asDRValue ].
	cfg initialBasicBlock jumpTo: basicBlock.

	"Guarantee we are in two address code, not in SSA anymore"	
	r1 result: r0 result.
	
	self generateCogitForInstruction: r1.	
	orASTInstruction := self statements first.

	self assert: orASTInstruction selector equals: #OrCq:R:.
	self assert: orASTInstruction arguments first value equals: 0.
	self assert: orASTInstruction arguments second name equals: 't0'
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateOrConstantWithNoResult [

	| cfg basicBlock r0 r1 orASTInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := R0 OR false"
		r1 := block or: r0 with: false asDRValue ].
	cfg initialBasicBlock jumpTo: basicBlock.

	"Guarantee we are in two address code, not in SSA anymore"	
	r1 result: DRNoRegister new.
	
	self generateCogitForInstruction: r1.	
	orASTInstruction := self statements first.

	self assert: orASTInstruction selector equals: #OrCq:R:.
	self assert: orASTInstruction arguments first value equals: 0.
	self assert: orASTInstruction arguments second name equals: 't0'
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateRotate [

	| cfg basicBlock rotateASTInstruction r2 |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		              | r0 |
		              r0 := block copy: 2.
		              r2 := block rotateLeft: r0 by: 1 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	"Guarantee we are in two address code, and we need no result value"
	r2 result: DRNoRegister new.

	self generateCogitForInstruction: r2.
	rotateASTInstruction := self statements first.
	"self RotateLeftCq: 1 R: t0"

	self assert: rotateASTInstruction selector equals: #RotateLeftCq:R:.
	self assert: rotateASTInstruction arguments first value equals: 1.
	self assert: rotateASTInstruction arguments second name equals: 't0'
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateSpillLoadInstruction [

	| cfg basicBlock spillRegister1 spillStoreInstruction storeInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		block copy: 2 ].
	cfg initialBasicBlock jumpTo: basicBlock.

	spillRegister1 := DRPhysicalGeneralPurposeRegister name: 'SPR1'.
	DRCogitLinearScanRegisterAllocator new
		integerRegisters: #();
		spillRegisters: { spillRegister1 };
		allocateRegistersIn: cfg.

	storeInstruction := basicBlock instructions second.
	self generateCogitForInstruction: storeInstruction.
	spillStoreInstruction := self statements first.
	self assert: spillStoreInstruction selector equals: #MoveR:Mw:r:.
	self assert: spillStoreInstruction arguments first name equals: spillRegister1 name.
	self assert: spillStoreInstruction arguments second value equals: (storeInstruction operand2 stackOffset - 1) * 8 "bytes".
	self assert: spillStoreInstruction arguments third name equals: 'SPReg'.
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateStagedFalseAsZero [

	| cfg basicBlock r0 compare |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		r0 := block copy: false asDRValue.
		r0 markAsStaged ].
	cfg initialBasicBlock jumpTo: basicBlock.

	self generateCogitForInstruction: r0.	
	compare := self statements first.

	"When staged, this should generate:
	
	t0 := 0.
	"
	self assert: compare isAssignment.	
	self assert: compare variable name equals: 't0'.
	self assert: compare value value equals: 0.
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateStagedNilAsZero [

	| cfg basicBlock r0 compare |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		r0 := block copy: nil asDRValue.
		r0 markAsStaged ].
	cfg initialBasicBlock jumpTo: basicBlock.

	self generateCogitForInstruction: r0.	
	compare := self statements first.

	"When staged, this should generate:
	
	t0 := 0.
	"
	self assert: compare isAssignment.	
	self assert: compare variable name equals: 't0'.
	self assert: compare value value equals: 0.
]

{ #category : 'tests' }
DRCogitGeneratorTest >> testGenerateStagedTrueAsOne [

	| cfg basicBlock r0 compare |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		r0 := block copy: true asDRValue.
		r0 markAsStaged ].
	cfg initialBasicBlock jumpTo: basicBlock.

	self generateCogitForInstruction: r0.	
	compare := self statements first.

	"When staged, this should generate:
	
	t0 := 1.
	"
	self assert: compare isAssignment.	
	self assert: compare variable name equals: 't0'.
	self assert: compare value value equals: 1.
]
