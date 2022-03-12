Class {
	#name : #DRSCCPConstantFoldingTest,
	#superclass : #DRSCCPTest,
	#category : #'Druid-Tests'
}

{ #category : #helpers }
DRSCCPConstantFoldingTest >> assertLastInstructionOf: cfg isCopyOf: instruction [

	self assert: cfg instructions allButLast last isCopy.
	self assert: cfg instructions allButLast last operand1 equals: instruction.
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> assertLastInstructionOf: cfg isCopyValue: aValue [

	self assertLastInstructionOf: cfg isCopyOf: aValue asDRValue.
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> copyOf: valueOrRegister [

	^ DRCopy operands: { valueOrRegister } result:  (DRTemporaryRegister id: 2)
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> copyOfAnyRegister [

	^ self copyOf: (DRTemporaryRegister id: 100)
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> nonReducibleOperation [

	^ DRGreaterThanComparison
		  operands: { 
				  (DRTemporaryRegister id: 98).
				  (DRTemporaryRegister id: 99) }
		  result: (DRTemporaryRegister id: 100)
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingAddInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRAdd between: 3 and: 4.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 7 asDRValue
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingAddOfLeftNonConstants [

	| r cfg |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRAdd between: r and: 3.

	self assert: cfg instructions third isAdd.
	self
		assert: cfg instructions third operand1 operands first
		equals: r.
	self
		assert: cfg instructions third operand2
		equals: 3 asDRValue
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingAddOfRightNonConstants [

	| r cfg |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRAdd between: 3 and: r.

	self assert: cfg instructions third isAdd.
	self
		assert: cfg instructions third operand1
		equals: 3 asDRValue.
	self
		assert: cfg instructions third operand2 operands first
		equals: r
]

{ #category : #arithmetic }
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

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantAndFalseIsFalse [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: 17 and: false.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantAndTrueIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: 17 and: true.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantOrFalseIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: 17 and: false.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantOrTrueIsTrue [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: 17 and: true.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseAndConstantIsFalse [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: false and: 17.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseAndOperationIsFalse [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DRAnd between: false andOp: op.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseAndRegisterIsFalse [

	| cfg reg |
	cfg := self testConstantFoldingOf: DRAnd between: false andOp: self copyOfAnyRegister.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseOrConstantIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: false and: 17.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseOrOperationIsOperation [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DROr between: false andOp: op.

	self assertLastInstructionOf: cfg isCopyOf: op.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseOrRegisterIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DROr between: false andOp: copy.

	self assertLastInstructionOf: cfg isCopyOf: copy operand1.
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroLeft [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: 0
		       and: (DRPhysicalRegister name: 'Rx').

	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroRight [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: (DRPhysicalRegister name: 'Rx')
		       and: 0.
	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRMultiply between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: 12
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction between: value1 and: value2 [

	^ self testConstantFoldingOf: instruction between: value1 andOp: (self copyOf: value2 asDRValue) 
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction between: value andOp: operand [

	| b1 cfg copy1 foldedInstruction |
	cfg := self setUpCFG: 1. 
	b1 := cfg b1.

	b1 addInstruction: (copy1 := DRCopy operands: { value asDRValue } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (operand).
	b1 addInstruction: (foldedInstruction := instruction operands: { copy1. operand } result:  (DRTemporaryRegister id: 3)).
	b1 addInstruction: (instructionFactory copy: foldedInstruction).

	optimisation applyTo: cfg.
	^ cfg
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingSubstractInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRSubtract between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: -1
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueAndConstantIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: true and: 17.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueAndOperationIsOperation [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DRAnd between: true andOp: op.

	self assertLastInstructionOf: cfg isCopyOf: op.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueAndRegisterIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DRAnd between: true andOp: copy.

	self assertLastInstructionOf: cfg isCopyOf: copy operand1.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueOrConstantIsTrue [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: true and: 17.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueOrOperationIsTrue [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DROr between: true andOp: op.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #boolean }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueOrRegisterIsTrue [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: true andOp: self copyOfAnyRegister.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithConstants [

	| cfg |
	cfg := self testConstantFoldingOf: DRBitShift between: 2r1000 and: 1.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 2r100 asDRValue
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithNonLeftConstant [

	| cfg r lastInstruction |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRBitShift between: r and: 1.

	lastInstruction := cfg instructions third.
	self assert: lastInstruction isLogicalRightShift.
	self assert: lastInstruction operand1 operand1 equals: r.
	self assert: lastInstruction operand2 value equals: 1.
]
