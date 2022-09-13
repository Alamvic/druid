Class {
	#name : #DRSCCPConstantPropagationTest,
	#superclass : #DRSCCPTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPConstantPropagationTest >> testDoNotPropagateInPhiFunctions [

	"Phi functions should contain variables and not values"

	| cfg copy1 copy2 phi |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf: (DRPhysicalRegister name: 'Parameter')).

	copy1 := cfg b2 copy: 1.
	copy2 := cfg b3 copy: 2.
	phi := cfg b4 phiWith: copy1 with: copy2.

	optimisation applyTo: cfg.

	self assert: cfg b4 firstInstruction equals: phi.
	self assert: phi operand1 equals: copy1.
	self assert: phi operand2 equals: copy2
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testDoNotPropagateLoadOperand [

	| cfg copy1 |
	cfg := self setUpCFG: 1.

	"T1 := LOAD 1"
	copy1 := cfg b1 load: 1.
	"T2 := T1"
	cfg b1 addInstruction: (instructionFactory copy: copy1).
	
	optimisation applyTo: cfg.

	"Keep T2 := T1"
	self assert: cfg instructions nextToLast operand1 equals: copy1
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateBetweenBasicBlocks [

	| cfg copy1 |
	cfg := self setUpCFG: 2.

	"T1 := 1"
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 1).
	
	"T2 := T1"
	cfg b2 addInstruction: (instructionFactory copy: copy1).
	
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: 1 asDRValue
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateToAddFixesUsers [

	| cfg copy1 add rewrittenAdd |
	cfg := self setUpCFG: 1.
		
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 1).
	
	"T1 := 1"
	"T2 := T1 + 0"
	cfg b1 addInstruction: (add := instructionFactory add: copy1 to: 0).

	optimisation applyTo: cfg.
	"T1 := 1"
	"T2 := 1"

	rewrittenAdd := cfg b1 instructions second.

	self assert: copy1 users isEmpty.
	self deny: (rewrittenAdd operands includes: copy1).
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateToAddOperand [

	| cfg copy1 |
	cfg := self setUpCFG: 1.
		
	"T1 := 1"
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 1).
	
	"T2 := Param + T1"
	cfg b1 addInstruction: (instructionFactory add: (DRPhysicalRegister name: 'Rx') to: copy1).
	
	optimisation applyTo: cfg.
	
	"Assert that we propagated the constant to the ADD operand"
	self assert: cfg instructions allButLast last isAdd.
	self assert: cfg instructions allButLast last operand2 equals: 1 asDRValue
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateToCopyOperand [

	| cfg copy1 |
	cfg := self setUpCFG: 1.

	"T1 := 1"
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 1).
	"T2 := T1"
	cfg b1 addInstruction: (instructionFactory copy: copy1).
	
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: 1 asDRValue
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateToEqualsFixesUsers [

	| cfg copy1 add |
	cfg := self setUpCFG: 1.
		
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 1).
	
	"T1 := 1"
	"T2 := EqualsTo T1 0"
	cfg b1 addInstruction: (add := instructionFactory equals: copy1 to: 0).

	optimisation applyTo: cfg.
	"T1 := 1"
	"T2 := EqualsTo 1 0"

	self assert: copy1 users isEmpty.
	self deny: (add operands includes: copy1).
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateWhenResultIsPhisicalRegister [

	"Phi functions should contain variables and not values"

	| cfg copy1 b1 b2 b3 copy2 joinBlock phi pr prcopy1 prcopy2 |
	cfg := DRControlFlowGraph new.
	pr := DRPhysicalRegister name: 'PR'.
	
	cfg initialBasicBlock jumpTo: (b1 := cfg newBasicBlock).
	b1 jumpIf: (DRPhysicalRegister name: 'Parameter') to: (b2 := cfg newBasicBlock) ifFalseTo: (b3 := cfg newBasicBlock).

	"T1 := 1.
	PR := T1"
	b2 addInstruction: (copy1 := instructionFactory copy: 1).
	b2 addInstruction: (prcopy1 := instructionFactory copy: copy1).
	prcopy1 result: pr.

	"T2 := 2.
	PR := T2"
	b3 addInstruction: (copy2 := instructionFactory copy: 2).
	b3 addInstruction: (prcopy2 := instructionFactory copy: copy2).
	prcopy2 result: pr.
	
	joinBlock := cfg newBasicBlock.
	b2 jumpTo: joinBlock.
	b3 jumpTo: joinBlock.
	joinBlock addInstruction: (phi := instructionFactory phiWith: copy1 with: copy2).

	optimisation applyTo: cfg.

	self assert: b2 instructions second operand1 equals: 1 asDRValue.
	self assert: b3 instructions second operand1 equals: 2 asDRValue.
]
