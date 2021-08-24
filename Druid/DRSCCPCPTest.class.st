Class {
	#name : #DRSCCPCPTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPCPTest >> setUpCFG: n [

	| cfg b0 b1 b2 b3 |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	
	cfg initialBasicBlock: b0.
	
	n = 1 ifTrue: [ 
		b0 endInstruction: DRNoop new.
		cfg initialBasicBlock: b0. 
		cfg b0: b0. ].
	
	n = 2 ifTrue: [ 
		b1 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		
		cfg b0: b0. 
		cfg b1: b1. ].

	n = 3 ifTrue: [ 
		b2 endInstruction: DRNoop new.
		b0 jumpTo: b1.
		b1 jumpTo: b2. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. ].
	
	n = 4 ifTrue: [ 
		b3 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		b1 jumpTo: b2. 
		b2 jumpTo: b3. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. 
		cfg b3: b3. ].

	^ cfg.
]

{ #category : #tests }
DRSCCPCPTest >> setUpCFGConditionnal: jump [

	| cfg b0 b1 b2 b3 b4|
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	b4 := DRBasicBlock id: 4. 
	
	cfg initialBasicBlock: b0.
	
	b4 endInstruction: DRNoop new. 
	b0 jumpTo: b1. 
	b1 endInstruction: jump. 
	jump trueBranch: b2.
	jump falseBranch: b3.
	b2 jumpTo: b4. 
	b3 jumpTo: b4. 
		
	cfg b0: b0.
	cfg b1: b1. 
	cfg b2: b2. 
	cfg b3: b3. 
	cfg b4: b4. 
	cfg conditionnalJump: jump. 

	^ cfg.
]

{ #category : #tests }
DRSCCPCPTest >> testPropagateAndFoldInComplexInstructionLeft [

	| optimisation b0 b1 cfg copy1 operandList|
	optimisation := DRSCCP new.
		
	cfg := self setUpCFG: 2. 
	b0 := cfg b0. 
	b1 := cfg b1. 

	"T1 := 1"
	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1 + 2"
	
	operandList := OrderedCollection new.
	operandList add: copy1. 
	operandList add: (DRConstantValue value: 1). 
	
	b1 addInstruction: (DRAdd operands: operandList result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 3).
	
]

{ #category : #tests }
DRSCCPCPTest >> testPropagateAndFoldInComplexInstructionRight [

	| optimisation b0 b1 cfg copy1 operandList|
	optimisation := DRSCCP new.
		
	cfg := self setUpCFG: 2. 
	b0 := cfg b0. 
	b1 := cfg b1. 
	
	"T1 := 1"
	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := 2 + T1"
	
	operandList := OrderedCollection new. 
	operandList add: (DRConstantValue value: 2). 
	operandList add: copy1. 
	
	b1 addInstruction: (DRAdd operands: operandList result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 3).
]

{ #category : #tests }
DRSCCPCPTest >> testPropagateBetweenBasicBlocks [

	| optimisation b1 cfg copy1 b2 b0 |
	optimisation := DRSCCP new.
	
	cfg := self setUpCFG: 3. 
	b0 := cfg b0. 
	b1 := cfg b1. 
	b2 := cfg b2. 

	"T1 := 1"
	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1"
	b2 addInstruction: (DRCopy operands: { copy1 } result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPCPTest >> testPropagateDifferentValuesInPhi [

	| optimisation b0 b1 cfg copy1 leftBlock rightBlock copy2 jump exitBasicBlock |
	optimisation := DRSCCP new.
	
	cfg := self setUpCFG: 4. 
	b0 := cfg b0. 
	b1 := cfg b1. 
	leftBlock := cfg b2.
	exitBasicBlock := cfg b3. 
	
	rightBlock := DRBasicBlock id: 4. 
	rightBlock jumpTo: exitBasicBlock. 
	
	"T1 := 1"
	leftBlock addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	
	"T2 := 2"
	rightBlock addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 2)).
	
	b1 endInstruction: (jump := DRConditionalJump operands: { DRConstantValue value: true }).
	jump trueBranch: leftBlock.
	jump falseBranch: rightBlock.
	
	"T3 := ø(T1, T2)"
	exitBasicBlock addInstruction: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).

	optimisation applyTo: cfg.
	
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPCPTest >> testPropagateInSingleBasicBlock [

	| optimisation b0 b1 cfg copy1 |
	optimisation := DRSCCP new.
	
	cfg := self setUpCFG: 2. 
	b0 := cfg b0. 
	b1 := cfg b1. 


	"T1 := 1"
	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1"
	b1 addInstruction: (DRCopy operands: { copy1 } result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPCPTest >> testPropagatePhiOnDifferentValues [

	| optimisation b0 b1 b3 cfg copy1 leftBlock rightBlock copy2 jump phiInstruction copyInstruction |
	optimisation := DRSCCP new.
	cfg := self setUpCFG: 4. 
	b0 := cfg b0. 
	b1 := cfg b1. 
	leftBlock := cfg b2.
	b3 := cfg b3. 
	
	rightBlock := DRBasicBlock id: 4. 
	rightBlock jumpTo: b3. 
	
	
	leftBlock := DRBasicBlock new.
	"T1 := 1"
	leftBlock addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	
	rightBlock := DRBasicBlock new.
	"T2 := 2"
	rightBlock addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 2)).
	
	b1 endInstruction: (jump := DRConditionalJump operands: { DRConstantValue value: true }).
	jump trueBranch: leftBlock.
	jump falseBranch: rightBlock.
	
	leftBlock jumpTo: b3.
	rightBlock jumpTo: b3. 
	
	"T3 := ø(T1, T2)"
	phiInstruction := ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).
	b3 addInstruction: phiInstruction. 
	
	copyInstruction := DRCopy operands: { phiInstruction } result: (DRTemporaryRegister id: 4). 
	b3 addInstruction: copyInstruction . 
	

	optimisation applyTo: cfg.
	
	"The T1 access got replaced by the contant 1"
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 1).
]

{ #category : #tests }
DRSCCPCPTest >> testPropagatePhiOnSameValue [

	| optimisation b0 b1 cfg copy1 leftBlock rightBlock copy2 jump phi exitBasicBlock |
	optimisation := DRSCCP new.
	
	cfg := self setUpCFG: 4. 
	b0 := cfg b0. 
	b1 := cfg b1. 
	leftBlock := cfg b2.
	exitBasicBlock := cfg b3. 
	
	rightBlock := DRBasicBlock id: 4. 
	rightBlock jumpTo: exitBasicBlock. 
	
	
	"T1 := 1"
	leftBlock addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	

	"T2 := 1"
	rightBlock addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 2)).
	
	b1 endInstruction: (jump := DRConditionalJump operands: { DRConstantValue value: true }).
	jump trueBranch: leftBlock.
	jump falseBranch: rightBlock.
	
	"T3 := ø(T1, T2)"
	phi := ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).
	exitBasicBlock addInstruction: phi. 
	"T4 := T3"
	exitBasicBlock addInstruction: (DRCopy operands: {phi} result: (DRTemporaryRegister id: 4)). 


	optimisation applyTo: cfg.
	
	"T4 := T3 should become T4 := 1"
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPCPTest >> testPropagateSameValuesInPhi [

	| optimisation b0 b1 cfg copy1 leftBlock rightBlock copy2 jump exitBasicBlock |
	optimisation := DRSCCP new.
	
	cfg := self setUpCFG: 4. 
	b0 := cfg b0. 
	b1 := cfg b1. 
	leftBlock := cfg b2.
	exitBasicBlock := cfg b3. 
	
	rightBlock := DRBasicBlock id: 4. 
	rightBlock jumpTo: exitBasicBlock. 
	
	"T1 := 1"
	leftBlock addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	
	"T2 := 1"
	rightBlock addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 2)).
	
	cfg initialBasicBlock endInstruction: (jump := DRConditionalJump operands: { DRConstantValue value: true }).
	jump trueBranch: leftBlock.
	jump falseBranch: rightBlock.
	
	"T3 := ø(T1, T2)"
	exitBasicBlock addInstruction: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).

	optimisation applyTo: cfg.
	
	"T3 := ø(T1, T2) should become T3 := 1"
	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 1)
]
