Class {
	#name : 'DRLocalVariableInstructionScheluderTest',
	#superclass : 'DRMethodOptimizationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'running' }
DRLocalVariableInstructionScheluderTest >> newCompiler [

	^ DRMethodCompilerCompiler new
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testOnLoops [

	| cfg variableName phi b5 |
	
	cfg := self setUpCFGWithLoop.
	b5 := cfg b1 predecessors first.
	phi := cfg phiFunctions unique.
	cfg b1 endInstruction replaceOperandAtIndex: 1 by: phi.

	variableName := 'ssa_', phi result id asString.	
	DRLocalVariableInstructionScheluder new applyTo: cfg.

	"Replace the Phi by a Load for the condition"
	self assert: cfg b1 instructions first isLoadTemporaryVariable. 
	self assert: cfg b1 instructions first operand1 value equals: variableName.
	
	"Store the initial value"
	self assert: b5 instructions second isStoreTemporaryVariable.
	self assert: b5 instructions second operand1 value equals: variableName.
	self assert: b5 instructions second operand2 equals: phi operand1.
	
	"Store inside the loop"
	self assert: cfg b2 endInstruction isBackJump.
	self assert: cfg b2 instructions second isStoreTemporaryVariable.
	self assert: cfg b2 instructions second operand1 value equals: variableName.
	self assert: cfg b2 instructions second operand2 equals: phi operand2.
	
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testOneUseDoNothing [

	| cfg t0 t1|
	
	cfg := self setUpCFG: 2.
	t0 := cfg b1 add: 1 to: 2.
	t1 := cfg b2 add: t0 to: 5.

	DRLocalVariableInstructionScheluder new applyTo: cfg.
	
	self assert: cfg b1 instructions first equals: t0.
	self assert: cfg b2 instructions first equals: t1.
	
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testPhiBecomeLoadTemp [

	| cfg variableName |
	cfg := self setUpCFGWithConditionalWithPhi.
	cfg b4 return: cfg phi.
	
	DRLocalVariableInstructionScheluder new applyTo: cfg.

	variableName := 'ssa_2'.
	
	self assert: cfg b2 instructions second isStoreTemporaryVariable.
	self
		assert: cfg b2 instructions second operand1 value
		equals: variableName.
	self
		assert: cfg b2 instructions second operand2
		equals: cfg b2 instructions first.

	self assert: cfg b3 instructions second isStoreTemporaryVariable.
	self
		assert: cfg b3 instructions second operand1 value
		equals: variableName.
	self
		assert: cfg b3 instructions second operand2
		equals: cfg b3 instructions first.

	self assert: cfg b4 instructions first isLoadTemporaryVariable.
	self assert: cfg b4 instructions first operand1 value equals: variableName.
	
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testPhiOneOperand [

	| cfg t1 |
	cfg := self setUpCFG: 2.

	t1 := cfg b1 copy: 1.
	cfg b2 phiWith: t1.

	DRLocalVariableInstructionScheluder new applyTo: cfg.

	self assert: cfg b2 instructions first isCopy.
	self assert: cfg b2 instructions first operand equals: t1
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testResultAreStored [

	| cfg t0 t1 t2 variableName|
	
	variableName := 'ssa_0'.
	cfg := self setUpCFG: 3.
	t0 := cfg b1 add: 1 to: 2.
	t1 := cfg b2 add: t0 to: 5.
	t2 := cfg b3 add: t0 to: 4.

	DRLocalVariableInstructionScheluder new applyTo: cfg.
	
	self assert: cfg b1 instructions second isStoreTemporaryVariable. 
	self assert: cfg b1 instructions second operand1 value equals: variableName.
	
	self assert: cfg b2 instructions first isLoadTemporaryVariable.
	self assert: cfg b2 instructions first operand1 value equals: variableName.
	self assert: cfg b2 instructions second operand1 equals: cfg b2 instructions first.
	
	self assert: cfg b3 instructions first isLoadTemporaryVariable.
	self assert: cfg b3 instructions first operand1 value equals: variableName.
	self assert: cfg b3 instructions second operand1 equals: cfg b3 instructions first.
	
]
