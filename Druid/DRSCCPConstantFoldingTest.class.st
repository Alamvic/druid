Class {
	#name : #DRSCCPConstantFoldingTest,
	#superclass : #DRSCCPTest,
	#category : #'Druid-Tests'
}

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> assertLastInstructionOf: cfg isCopyValue: aValue [

	self assert: cfg instructions allButLast last isCopy.
	self assert: cfg instructions allButLast last operand1 equals: aValue asDRValue.
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingAddInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRAdd between: 3 and: 4.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 7 asDRValue
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingAddOfLeftNonConstants [

	| r cfg |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRAdd between: r and: 3.

	self assert: cfg instructions allButLast last isAdd.
	self
		assert: cfg instructions allButLast last operand1 operands first
		equals: r.
	self
		assert: cfg instructions allButLast last operand2
		equals: 3 asDRValue
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingAddOfRightNonConstants [

	| r cfg |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRAdd between: 3 and: r.

	self assert: cfg instructions allButLast last isAdd.
	self
		assert: cfg instructions allButLast last operand1
		equals: 3 asDRValue.
	self
		assert: cfg instructions allButLast last operand2 operands first
		equals: r
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingBitAndInSingleBasicBlock [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRBitAnd
		       between: 2r0101010110
		       and: 2r0101011111.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 2r0101010110 asDRValue
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroLeft [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: 0
		       and: (DRPhysicalRegister name: 'Rx').

	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroRight [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: (DRPhysicalRegister name: 'Rx')
		       and: 0.
	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRMultiply between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: 12
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction between: operand1 and: operand2 [

	| b0 b1 cfg copy1 copy2 |
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { operand1 asDRValue } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { operand2 asDRValue } result:  (DRTemporaryRegister id: 2)).
	b1 addInstruction: (instruction operands: { copy1. copy2 } result:  (DRTemporaryRegister id: 3)).

	DRSCCP new applyTo: cfg.
	^ cfg
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testConstantFoldingSubstractInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRSubtract between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: -1
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithConstants [

	| cfg |
	cfg := self testConstantFoldingOf: DRBitShift between: 2r1000 and: 1.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 2r100 asDRValue
]

{ #category : #'tests - folding' }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithNonLeftConstant [

	| cfg r lastInstruction |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRBitShift between: r and: 1.

	lastInstruction := cfg instructions allButLast last.
	self assert: lastInstruction isLogicalRightShift.
	self assert: lastInstruction operand1 operand1 equals: r.
	self assert: lastInstruction operand2 value equals: 1.
]
