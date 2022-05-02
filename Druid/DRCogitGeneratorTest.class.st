Class {
	#name : #DRCogitGeneratorTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #helpers }
DRCogitGeneratorTest >> generateCogitForInstruction: aDRInstruction [
	
	| generator |
	generator := DRCogitCodeGenerator new.
	aDRInstruction acceptVisitor: generator.
	^ generator
]

{ #category : #tests }
DRCogitGeneratorTest >> testConstantsBiggerThan5000AreInHexa [

	| cfg basicBlock r0 cogitAST move2ACASTInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block |
		"R0 := 2"
		r0 := block copy: 8000.].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"Transform 3AC to 2AC =>
	R0 := 16r1F40.
	"
	cogitAST := self generateCogitForInstruction: r0.
	
	move2ACASTInstruction := cogitAST generatorMethodBuilder statements first.
	self assert: move2ACASTInstruction selector equals: #MoveCq:R:.
	self assert: move2ACASTInstruction arguments first value equals: 8000.
	self assert: move2ACASTInstruction arguments first sourceText equals: '16r1F40'.
	self assert: move2ACASTInstruction arguments second name equals: 'R0'
]

{ #category : #tests }
DRCogitGeneratorTest >> testGenerateDivision [

	| cfg basicBlock r1 cogitAST divideASTInstruction r2 |
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

	cogitAST := self generateCogitForInstruction: r2.	
	divideASTInstruction := cogitAST generatorMethodBuilder statements first.
	self assert: divideASTInstruction selector equals: #DivR:R:Quo:Rem:.
	self assert: divideASTInstruction arguments first name equals: 'R1'.
	self assert: divideASTInstruction arguments second name equals: 'R0'.
	self assert: divideASTInstruction arguments third name equals: 'R2'.
	self assert: divideASTInstruction arguments fourth name equals: 'R3'.
]

{ #category : #tests }
DRCogitGeneratorTest >> testGenerateModulo [

	| cfg basicBlock r1 cogitAST divideASTInstruction r2 |
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

	cogitAST := self generateCogitForInstruction: r2.	
	divideASTInstruction := cogitAST generatorMethodBuilder statements first.
	self assert: divideASTInstruction selector equals: #DivR:R:Quo:Rem:.
	self assert: divideASTInstruction arguments first name equals: 'R1'.
	self assert: divideASTInstruction arguments second name equals: 'R0'.
	self assert: divideASTInstruction arguments third name equals: 'R3'.
	self assert: divideASTInstruction arguments fourth name equals: 'R2'.
]
