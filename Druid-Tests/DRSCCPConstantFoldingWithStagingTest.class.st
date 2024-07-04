Class {
	#name : #DRSCCPConstantFoldingWithStagingTest,
	#superclass : #DRSCCPConstantFoldingTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #running }
DRSCCPConstantFoldingWithStagingTest >> setUp [

	super setUp.
	optimisation := DRSCCPWithStaging new
]

{ #category : #arithmetic }
DRSCCPConstantFoldingWithStagingTest >> testConstantFoldingAddJitCompileExpressionWithNonJitCompileExpression [

	self
		testNonConstantFoldingOf: DRAdd
		between: self varJITExpression
		and: self nonReducibleOperation
]

{ #category : #register }
DRSCCPConstantFoldingWithStagingTest >> testConstantFoldingFalseOrRegisterIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DROr between: false andOp: copy.

	"When staging, the temporary registers are considered constants and this can be folded"
	self assertLastInstructionOf: cfg isCopyOf: copy operand1
]

{ #category : #arithmetic }
DRSCCPConstantFoldingWithStagingTest >> testConstantFoldingLoadNonConstant [

	| cfg |
	cfg := self testConstantFoldingOf: DRLoad betweenOp: DRPointerType new and: self nonReducibleOperation.

	self assert: cfg instructions second isLoad.
	self assert: cfg instructions second size equals: 8.
	self assert: cfg instructions second operand1 isMemoryAddress
]

{ #category : #tests }
DRSCCPConstantFoldingWithStagingTest >> testConstantFoldingRegisterAndTrueIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DRAnd betweenOp: copy and: true.

	"When staging, the temporary registers are considered constants and this can be folded"
	self assertLastInstructionOf: cfg isCopyOf: copy operand1
]

{ #category : #register }
DRSCCPConstantFoldingWithStagingTest >> testConstantFoldingRegisterOrFalseIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DROr betweenOp: copy and: false.

	"When staging, the temporary registers are considered constants and this can be folded"
	self assertLastInstructionOf: cfg isCopyOf: copy operand1.
]

{ #category : #register }
DRSCCPConstantFoldingWithStagingTest >> testConstantFoldingTrueAndRegisterIsRegister [

	| cfg copy |
	copy := self copyOfAnyRegister.
	cfg := self testConstantFoldingOf: DRAnd between: true andOp: copy.

	"When staging, the temporary registers are considered constants and this can be folded"
	self assertLastInstructionOf: cfg isCopyOf: copy operand1.
]

{ #category : #tests }
DRSCCPConstantFoldingWithStagingTest >> testFoldingLoadWithConstant [

	| cfg load copyOfCopy |
	cfg := self setUpCFG: 1.

	"T1 := LOAD 1"
	load := cfg b1 load: 1.
	"T2 := T1"
	copyOfCopy := cfg b1 copy: load.

	optimisation applyTo: cfg.

	"T2 := LOAD_JIT_COMPILE_EXPRESSION 1"
	self assert: copyOfCopy operand1 isInterpreterReference.
	self
		assert: copyOfCopy operand1 reference
		equals: '(coInterpreter int64AtPointer: 1)'
]

{ #category : #tests }
DRSCCPConstantFoldingWithStagingTest >> testFoldingLoadWithOperation [

	| cfg load op copyOfLoad |
	cfg := self setUpCFG: 1.

	"T1 := op"
	cfg b1 addInstruction: (op := self nonReducibleOperation).
	"T2 := LOAD T1"
	load := cfg b1 load: op.
	"T3 := T2"
	copyOfLoad := cfg b1 copy: load.

	optimisation applyTo: cfg.

	"T3 := T2"
	self assert: copyOfLoad operand1 equals: load
]

{ #category : #tests }
DRSCCPConstantFoldingWithStagingTest >> testFoldingObjectReferenceIntoJITExpression [

	| cfg copy addition |
	cfg := self setUpCFG: 1.

	cfg b1 addInstruction: (copy := DRCopy
			         operands:
			         { (DRInterpreterReference reference: 'methodObj') }
			         result: (DRSSARegister name: 999)).
	addition := cfg b1 add: copy to: 1.
	cfg b1 return: addition.

	optimisation applyTo: cfg.

	"Object references should keep to be annotated for GC and not folded"
	self assert: cfg instructions first equals: copy.
	self assert: cfg instructions second isAdd.
	self assert: cfg instructions second operand1 equals: copy
]

{ #category : #tests }
DRSCCPConstantFoldingWithStagingTest >> testNonConstantFoldingOf: aClass between: aDROperand1 and: aDROperand2 [ 
	| cfg |
	cfg := self testConstantFoldingOf: aClass between: aDROperand1 and: aDROperand2.
	
	self assert: cfg instructions third class equals: aClass.
	self
		assert: cfg instructions third operand1 simpleConstantFold 
		equals: aDROperand1.
	self
		assert: cfg instructions third operand2 simpleConstantFold 
		equals: aDROperand2
]
