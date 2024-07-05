Class {
	#name : #DRSCCPConstantFoldingTest,
	#superclass : #DRSCCPTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #assertions }
DRSCCPConstantFoldingTest >> assertLastInstructionOf: cfg isCopyOf: instruction [

	self assert: cfg instructions fourth isCopy.
	self assert: cfg instructions fourth operand1 equals: instruction.
]

{ #category : #assertions }
DRSCCPConstantFoldingTest >> assertLastInstructionOf: cfg isCopyValue: aValue [

	self assertLastInstructionOf: cfg isCopyOf: aValue asDRValue.
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> copyOfAnyRegister [

	^ instructionFactory copy: (DRSSARegister name: 87)
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> nonReducibleOperation [

	^ DRGetConditionCode
		  operands: { 
				  DRGreaterThanComparison new.
				  (DRSSARegister name: 98).
				  (DRSSARegister name: 99) }
		  result: (DRSSARegister name: 100)
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingAddInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRAdd between: 3 and: 4.

	self assert: cfg instructions third isCopy.
	self
		assert: cfg instructions third operand1
		equals: 7 asDRValue
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingAddOfLeftNonConstants [

	| r cfg |
	r := DRPhysicalGeneralPurposeRegister name: 'Parameter'.
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
	r := DRPhysicalGeneralPurposeRegister name: 'Parameter'.
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

	self assert: cfg instructions third isCopy.
	self
		assert: cfg instructions third operand1
		equals: 2r0101010110 asDRValue
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantAndFalseIsFalse [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: 17 and: false.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantAndTrueIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: 17 and: true.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantOrFalseIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: 17 and: false.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingConstantOrTrueIsTrue [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: 17 and: true.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseAndConstantIsFalse [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: false and: 17.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseAndOperationIsFalse [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DRAnd between: false andOp: op.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseAndRegisterIsFalse [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: false andOp: self copyOfAnyRegister.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseOrConstantIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: false and: 17.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseOrOperationIsOperation [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DROr between: false andOp: op.

	self assertLastInstructionOf: cfg isCopyOf: op.
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingFalseOrRegisterIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DROr between: false andOp: copy.

	self assertLastInstructionOf: cfg isCopyOf: copy
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingLoadConstant [

	| b1 cfg operand1 operand2 |
	cfg := self setUpCFG: 1.
	b1 := cfg b1.

	operand1 := b1 copy: 9999.
	operand2 := b1 loadSInt64: operand1.

	optimisation applyTo: cfg.

	self assert: cfg instructions first isCopy.
	self assert: cfg instructions second isLoad
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingLoadJitCompileExpression [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRLoad
		       betweenOp: DRPointerType new
		       and: self varJITExpression.

	self assert: cfg instructions first isCopy.
	self assert: cfg instructions second isLoad.
	self
		assert: cfg instructions second address expression
		equals: cfg instructions first
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingLoadNonConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DRLoad betweenOp: DRPointerType new and: self nonReducibleOperation.

	self assert: cfg instructions second isLoad.
	self assert: cfg instructions second size equals: 8.
	self assert: cfg instructions second operand1 isMemoryAddress
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroLeft [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: 0
		       and: (DRPhysicalGeneralPurposeRegister name: 'Rx').

	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroRight [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: (DRPhysicalGeneralPurposeRegister name: 'Rx')
		       and: 0.
	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRMultiply between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: 12
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyWithFoldedUsers [

	| b1 cfg foldedInstruction operand1 operand2 |
	cfg := self setUpCFG: 1.
	b1 := cfg b1.

	operand1 := b1 copy:3.
	operand2 := b1 copy: 4.
	foldedInstruction := b1 multiply: operand1 by: operand2.
	b1 add: foldedInstruction to: 3.

	optimisation applyTo: cfg.

	self assertLastInstructionOf: cfg isCopyValue: 15
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction between: value1 and: value2 [

	^ self
		  testConstantFoldingOf: instruction
		  between: value1
		  andOp: (instructionFactory copy: value2)
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction between: value andOp: operand [

	^ self
		  testConstantFoldingOf: instruction
		  betweenOp: (instructionFactory copy: value)
		  andOp: operand
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction betweenOp: operand and: value [

	^ self
		  testConstantFoldingOf: instruction
		  betweenOp: operand
		  andOp: (instructionFactory copy: value)
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction betweenOp: operand1 andOp: operand2 [

	| b1 cfg foldedInstruction |
	cfg := self setUpCFG: 1.
	b1 := cfg b1.

	operand1 isInstruction ifTrue: [ b1 addInstruction: operand1 ].
	operand2 isInstruction ifTrue: [ b1 addInstruction: operand2 ].
	b1 addInstruction: (foldedInstruction := instruction
			                      operands: {
					                      operand1.
					                      operand2 }
			                      result:
			                      instructionFactory allocateTemporaryRegister).
	b1 addInstruction: (instructionFactory copy: foldedInstruction).
	optimisation applyTo: cfg.
	^ cfg
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingOperationAndFalseIsFalse [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DRAnd betweenOp: op and: false.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingOperationAndTrueIsOperation [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DRAnd betweenOp: op and: true.

	self assertLastInstructionOf: cfg isCopyOf: op.
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingOperationOrFalseIsOperation [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DROr betweenOp: op and: false.

	self assertLastInstructionOf: cfg isCopyOf: op.
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingOperationOrTrueIsTrue [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DROr betweenOp: op and: true.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingRegisterAndFalseIsFalse [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DRAnd betweenOp: copy and: false.

	self assertLastInstructionOf: cfg isCopyValue: false.
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingRegisterAndTrueIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DRAnd betweenOp: copy and: true.

	self assertLastInstructionOf: cfg isCopyOf: copy
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingRegisterOrFalseIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DROr betweenOp: copy and: false.

	self assertLastInstructionOf: cfg isCopyOf: copy
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingRegisterOrTrueIsTrue [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DROr betweenOp: copy and: true.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testConstantFoldingSubstractInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRSubtract between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: -1
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueAndConstantIsConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DRAnd between: true and: 17.

	self assertLastInstructionOf: cfg isCopyValue: 17.
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueAndOperationIsOperation [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DRAnd between: true andOp: op.

	self assertLastInstructionOf: cfg isCopyOf: op.
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueAndRegisterIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DRAnd between: true andOp: copy.

	self assertLastInstructionOf: cfg isCopyOf: copy.
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueOrConstantIsTrue [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: true and: 17.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueOrOperationIsTrue [

	| cfg op |
	op := self nonReducibleOperation.
	cfg := self testConstantFoldingOf: DROr between: true andOp: op.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #register }
DRSCCPConstantFoldingTest >> testConstantFoldingTrueOrRegisterIsTrue [

	| cfg |
	cfg := self testConstantFoldingOf: DROr between: true andOp: self copyOfAnyRegister.

	self assertLastInstructionOf: cfg isCopyValue: true.
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithConstants [

	| cfg |
	cfg := self testConstantFoldingOf: DRBitShift between: 2r1000 and: 1.

	self assert: cfg instructions third isCopy.
	self
		assert: cfg instructions third operand1
		equals: 2r100 asDRValue
]

{ #category : #arithmetic }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithNonLeftConstant [

	| cfg r lastInstruction |
	r := DRPhysicalGeneralPurposeRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRBitShift between: r and: 1.

	lastInstruction := cfg instructions third.
	self assert: lastInstruction isLogicalRightShift.
	self assert: lastInstruction operand1 operand1 equals: r.
	self assert: lastInstruction operand2 value equals: 1.
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testFoldingLoadWithConstant [

	| cfg load |
	cfg := self setUpCFG: 1.

	"T1 := LOAD 1"
	load := cfg b1 load: 1.
	"T2 := T1"
	cfg b1 copy: load.

	optimisation applyTo: cfg.

	self assert: cfg instructions second isCopy.
	self assert: cfg instructions second operand1 equals: load
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testFoldingLoadWithOperation [

	| cfg load op |
	cfg := self setUpCFG: 1.

	"T1 := op"
	cfg b1 addInstruction: (op := self nonReducibleOperation).
	"T2 := LOAD T1"
	load := cfg b1 load: op.
	"T3 := T2"
	cfg b1 copy: load.

	optimisation applyTo: cfg.

	"T3 := T2"
	self assert: cfg instructions third operand1 equals: load
]

{ #category : #constant }
DRSCCPConstantFoldingTest >> testFoldingObjectReferenceIntoJITExpression [

	| cfg copy addition |
	cfg := self setUpCFG: 1.

	cfg b1 addInstruction: (copy := DRCopy
			         operands:
			         { (DRInterpreterReference reference: 'methodObj') }
			         result: (DRSSARegister name: 999)).
	addition := cfg b1 add: copy to: 1.
	cfg b1 return: addition.

	optimisation applyTo: cfg.

	"Object references should keep to be annotated for GC"
	self assert: cfg instructions first equals: copy.
	"If no staging happens, everything stays the same"
	self assert: cfg instructions second isAdd.
	self
		assertCollection: cfg instructions second operands
		hasSameElements: {
				copy.
				1 asDRValue }
]

{ #category : #operation }
DRSCCPConstantFoldingTest >> varJITExpression [

	^ DRInterpreterReference reference: 'var'
]
