Class {
	#name : #DRSCCPCFTest,
	#superclass : #TestCase,
	#instVars : [
		'n'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPCFTest >> setUpCFG: n [

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
DRSCCPCFTest >> setUpCFGConditionnal: jump [

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
	b1 addInstruction: jump condition.
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
DRSCCPCFTest >> testConstantFoldingAddInSingleBasicBlock [

	| optimisation b0 b1 cfg copy1 copy2 operandList|
	optimisation := DRSCCP new.
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 3 } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 4 } result:  (DRTemporaryRegister id: 2)).
	
	operandList := OrderedCollection new. 
	operandList add: copy1. 
	operandList add: copy2.

	b1 addInstruction: (DRAdd  operands: operandList result:  (DRTemporaryRegister id: 3)).

	optimisation applyTo: cfg.
	

	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 7).
	self assert: cfg instructions allButLast last isCopy. 
]

{ #category : #tests }
DRSCCPCFTest >> testConstantFoldingBitAndInSingleBasicBlock [

	| optimisation b0 b1 cfg copy1 copy2 operandList|
	optimisation := DRSCCP new.
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 2r0101010110 } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 2r0101011111 } result:  (DRTemporaryRegister id: 2)).
	
	operandList := OrderedCollection new. 
	operandList add: copy1. 
	operandList add: copy2.

	b1 addInstruction: (DRBitAnd  operands: operandList result:  (DRTemporaryRegister id: 3)).
	optimisation applyTo: cfg.
	

	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 2r0101010110)
]

{ #category : #tests }
DRSCCPCFTest >> testConstantFoldingBitShiftInSingleBasicBlock [

	| optimisation b0 b1 cfg copy1 copy2 operandList|
	optimisation := DRSCCP new.
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 2r1000 } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 1} result:  (DRTemporaryRegister id: 2)).
	
	operandList := OrderedCollection new. 
	operandList add: copy1. 
	operandList add: copy2.

	b1 addInstruction: (DRBitShift  operands: operandList result:  (DRTemporaryRegister id: 3)).
	optimisation applyTo: cfg.
	

	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 2r100)
]

{ #category : #tests }
DRSCCPCFTest >> testConstantFoldingMultiplyInSingleBasicBlock [

	| optimisation b0 b1 cfg copy1 copy2 operandList|
	
	optimisation := DRSCCP new.
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 3 } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 4 } result:  (DRTemporaryRegister id: 2)).
	
	operandList := OrderedCollection new. 
	operandList add: copy1. 
	operandList add: copy2.

	b1 addInstruction: (DRMultiply operands: operandList result:  (DRTemporaryRegister id: 3)).
	optimisation applyTo: cfg.

	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 12)
]

{ #category : #tests }
DRSCCPCFTest >> testConstantFoldingMultiplyInSingleBasicBlockWithZeroValue [

	| optimisation b0 b1 cfg copy1 copy2 operandList|
	
	optimisation := DRSCCP new.
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 0 } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 4} result:  (DRTemporaryRegister id: 2)).
	
	operandList := OrderedCollection new. 
	operandList add: copy1. 
	operandList add: copy2.

	b1 addInstruction: (DRMultiply operands: operandList result:  (DRTemporaryRegister id: 3)).
	optimisation applyTo: cfg.
	

	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: 0)
]

{ #category : #tests }
DRSCCPCFTest >> testConstantFoldingSubstractInSingleBasicBlock [

	| optimisation b0 b1 cfg copy1 copy2 operandList|
	
	optimisation := DRSCCP new.
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 3 } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 4 } result:  (DRTemporaryRegister id: 2)).
	
	operandList := OrderedCollection new. 
	operandList add: copy1. 
	operandList add: copy2.

	b1 addInstruction: (DRSubstract operands: operandList result:  (DRTemporaryRegister id: 3)).
	optimisation applyTo: cfg.
	

	self assert: cfg instructions allButLast last operand1 equals: (DRConstantValue value: -1)
]

{ #category : #tests }
DRSCCPCFTest >> testSCCPPropagationLeft [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is false. 
	Only the block3 b3 is optimized."

	| cfg b0 b1 b2 b3 b4 optimisation copy1 copy2 copy3 copy4 copy5 copy6 jump b2OperandList b3OperandList |
	optimisation := DRSCCP new.

	copy1 := DRCopy
		         operands: { (DRConstantValue value: 1) }
		         result: (DRTemporaryRegister id: 1).
	copy2 := DRCopy
		         operands: { (DRConstantValue value: 2) }
		         result: (DRTemporaryRegister id: 2).
	jump := DRConditionalJump operands: { (DRLessOrEqualsThanComparison
			         operands: { 
					         copy1.
					         copy2 }
			         result: (DRTemporaryRegister id: 10)) }.


	cfg := self setUpCFGConditionnal: jump.
	b0 := cfg b0.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
	b4 := cfg b4.


	b1 addInstruction: copy1.
	b1 addInstruction: copy2.

	b2 addInstruction: (copy3 := DRCopy
			          operands: { (DRConstantValue value: 3) }
			          result: (DRTemporaryRegister id: 3)).
	b2 addInstruction: (copy4 := DRCopy
			          operands: { (DRConstantValue value: 4) }
			          result: (DRTemporaryRegister id: 4)).
	b2OperandList := OrderedCollection new.
	b2OperandList add: copy3.
	b2OperandList add: copy4.
	b2 addInstruction:
		(DRAdd operands: b2OperandList result: (DRTemporaryRegister id: 7)).

	b3 addInstruction: (copy5 := DRCopy
			          operands: { (DRConstantValue value: 5) }
			          result: (DRTemporaryRegister id: 5)).
	b3 addInstruction: (copy6 := DRCopy
			          operands: { (DRConstantValue value: 6) }
			          result: (DRTemporaryRegister id: 6)).
	b3OperandList := OrderedCollection new.
	b3OperandList add: copy5.
	b3OperandList add: copy6.
	b3 addInstruction:
		(DRAdd operands: b3OperandList result: (DRTemporaryRegister id: 8)).

	optimisation applyTo: cfg.


	self
		assert: (b2 instructions at: 3) operand1
		equals: (DRConstantValue value: 7).
	self assert: (b3 instructions at: 3) class equals: DRAdd
]

{ #category : #tests }
DRSCCPCFTest >> testSCCPPropagationLeftWithPhiFunction [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block3 b3 will not be called because the condition is true. 
	Only the block2 b2 is optimized."
	| cfg b0 b1 b2 b3 b4 optimisation copy1 copy2 copy3 copy4 copy5 copy6 jump b2OperandList b3OperandList phiInstruction add1 add2|
	optimisation := DRSCCP new.

	copy1 := DRCopy
		         operands: { (DRConstantValue value: 1) }
		         result: (DRTemporaryRegister id: 1).
	copy2 := DRCopy
		         operands: { (DRConstantValue value: 2) }
		         result: (DRTemporaryRegister id: 2).
	jump := DRConditionalJump operands:
		        { (DRLessOrEqualsThanComparison new operands: { 
				         copy1.
				         copy2 }) }.


	cfg := self setUpCFGConditionnal: jump.
	b0 := cfg b0.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
	b4 := cfg b4.


	b1 addInstruction: copy1.
	b1 addInstruction: copy2.

	b2 addInstruction: (copy3 := DRCopy
			          operands: { copy1 }
			          result: (DRTemporaryRegister id: 3)).
	b2 addInstruction: (copy4 := DRCopy
			          operands: { (DRConstantValue value: 4) }
			          result: (DRTemporaryRegister id: 4)).
	b2OperandList := OrderedCollection new.
	b2OperandList add: copy3.
	b2OperandList add: copy4.
	b2 addInstruction:
		(add1 := DRAdd operands: b2OperandList result: (DRTemporaryRegister id: 7)).

	b3 addInstruction: (copy5 := DRCopy
			          operands: { copy2 }
			          result: (DRTemporaryRegister id: 5)).
	b3 addInstruction: (copy6 := DRCopy
			          operands: { (DRConstantValue value: 6) }
			          result: (DRTemporaryRegister id: 6)).
	b3OperandList := OrderedCollection new.
	b3OperandList add: copy1.
	b3OperandList add: copy6.
	b3 addInstruction:
		(add2 := DRAdd operands: b3OperandList result: (DRTemporaryRegister id: 8)).
		
	b4 addInstruction: (phiInstruction := ((DRPhiFunction joinedVariables: { add1. add2 }) result: (DRTemporaryRegister id: 9); yourself)).

	optimisation applyTo: cfg.

	cfg instructions. 
	self assert: b4 instructions allButLast last operand1 value equals: 5. 
]

{ #category : #tests }
DRSCCPCFTest >> testSCCPPropagationRight [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is false. 
	Only the block3 b3 is optimized."

	| cfg b0 b1 b2 b3 b4 optimisation copy1 copy2 copy3 copy4 copy5 copy6 jump |
	optimisation := DRSCCP new.

	copy1 := DRCopy
		         operands: { (DRConstantValue value: 2) }
		         result: (DRTemporaryRegister id: 1).
	copy2 := DRCopy
		         operands: { (DRConstantValue value: 1) }
		         result: (DRTemporaryRegister id: 2).
	jump := DRConditionalJump operands:
		        { (DRLessOrEqualsThanComparison new operands: { 
				         copy1.
				         copy2 }) }.


	cfg := self setUpCFGConditionnal: jump.
	b0 := cfg b0.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
	b4 := cfg b4.


	b1 addInstruction: copy1.
	b1 addInstruction: copy2.

	b2 addInstruction: (copy3 := DRCopy
			          operands: { (DRConstantValue value: 3) }
			          result: (DRTemporaryRegister id: 3)).
	b2 addInstruction: (copy4 := DRCopy
			          operands: { (DRConstantValue value: 4) }
			          result: (DRTemporaryRegister id: 4)).
	b2 addInstruction: (DRAdd
			 operands: { 
					 copy3.
					 copy4 }
			 result: (DRTemporaryRegister id: 7)).

	b3 addInstruction: (copy5 := DRCopy
			          operands: { (DRConstantValue value: 5) }
			          result: (DRTemporaryRegister id: 5)).
	b3 addInstruction: (copy6 := DRCopy
			          operands: { (DRConstantValue value: 6) }
			          result: (DRTemporaryRegister id: 6)).
	b3 addInstruction: (DRAdd
			 operands: { 
					 copy5.
					 copy6 }
			 result: (DRTemporaryRegister id: 8)).

	optimisation applyTo: cfg.


	self
		assert: (b3 instructions at: 3) operand1
		equals: (DRConstantValue value: 11).
	self assert: (b2 instructions at: 3) class equals: DRAdd
]
