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
DRCogitGeneratorTest >> testGenerateAddTranslatesTo2AC [

	| cfg basicBlock r1 cogitAST move2ACASTInstruction add2ACASTInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := R0 + 1"
		r1 := block add: r0 to: 1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"Transform 3AC to 2AC =>
	R0 := 2.
	R1 := R0.
	R1 := R1 + 1
	"
	cogitAST := self generateCogitForInstruction: r1.
	
	move2ACASTInstruction := cogitAST generatorMethodBuilder statements first.
	self assert: move2ACASTInstruction selector equals: #MoveR:R:.
	self assert: move2ACASTInstruction arguments first name equals: 'R0'.
	self assert: move2ACASTInstruction arguments second name equals: 'R1'.
	
	add2ACASTInstruction := cogitAST generatorMethodBuilder statements second.
	self assert: add2ACASTInstruction selector equals: #AddCq:R:.
	self assert: add2ACASTInstruction arguments first value equals: 1.
	self assert: add2ACASTInstruction arguments second name equals: 'R1'.
]

{ #category : #tests }
DRCogitGeneratorTest >> testGenerateBitAndTranslatesTo2AC [

	| cfg basicBlock r1 cogitAST move2ACASTInstruction add2ACASTInstruction |
	cfg := DRControlFlowGraph new.
	basicBlock := cfg newBasicBlockWith: [ :block | | r0 |
		"R0 := 2"
		r0 := block copy: 2.
		"R1 := R0 + 1"
		r1 := block bitAnd: r0 with: 1 ].
	cfg initialBasicBlock jumpTo: basicBlock.
	
	"Transform 3AC to 2AC =>
	R0 := 2.
	R1 := R0.
	R1 := R1 & 1
	"
	cogitAST := self generateCogitForInstruction: r1.
	
	move2ACASTInstruction := cogitAST generatorMethodBuilder statements first.
	self assert: move2ACASTInstruction selector equals: #MoveR:R:.
	self assert: move2ACASTInstruction arguments first name equals: 'R0'.
	self assert: move2ACASTInstruction arguments second name equals: 'R1'.
	
	add2ACASTInstruction := cogitAST generatorMethodBuilder statements second.
	self assert: add2ACASTInstruction selector equals: #AndCq:R:.
	self assert: add2ACASTInstruction arguments first value equals: 1.
	self assert: add2ACASTInstruction arguments second name equals: 'R1'.
]
