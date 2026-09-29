Class {
	#name : 'DRLocalVariableInstructionScheluderTest',
	#superclass : 'DRMethodOptimizationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'running' }
DRLocalVariableInstructionScheluderTest >> newCompiler [

	^ DRMethodCompilerCompiler new
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testOuterCopyOperandIsMovedToTemp [

	| cfg t1 load loadName copy store |
	cfg := self setUpCFG: 2.

	t1 := cfg b1 copy: 1.
	cfg b2 copy: t1.

	DRLocalVariableInstructionScheluder new applyTo: cfg.

	load := cfg b2 instructions first.
	self assert: load isLoadTemporaryVariable.
	loadName := load variableName.

	copy := cfg b2 instructions second.
	self assert: copy isCopy.
	self assert: copy operand1 equals: load.
	
	store := cfg b1 instructions second.
	self assert: store isStoreTemporaryVariable.
	self assert: store variableName equals: loadName
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testPhiOneOperand [

	| cfg t1 load loadName copy store |
	cfg := self setUpCFG: 2.

	t1 := cfg b1 copy: 1.
	cfg b2 phiWith: t1.

	DRLocalVariableInstructionScheluder new applyTo: cfg.

	load := cfg b2 instructions first.
	self assert: load isLoadTemporaryVariable.
	loadName := load variableName.

	copy := cfg b2 instructions second.
	self assert: copy isCopy.
	self assert: copy operand1 equals: load.
	
	store := cfg b1 instructions second.
	self assert: store isStoreTemporaryVariable.
	self assert: store variableName equals: loadName
]

{ #category : 'tests' }
DRLocalVariableInstructionScheluderTest >> testResultAreStored [

	| cfg t0 t1 t2 firstStoreName secondStoreName |
	
	cfg := self setUpCFG: 3.
	t0 := cfg b1 add: 1 to: 2.
	t1 := cfg b2 add: t0 to: 5.
	t2 := cfg b3 add: t0 to: 4.

	DRLocalVariableInstructionScheluder new applyTo: cfg.
	
	firstStoreName := cfg b1 instructions second variableName.
	secondStoreName := cfg b1 instructions third variableName.
	self assert: cfg b1 instructions second isStoreTemporaryVariable. 
	
	self assert: cfg b2 instructions first isLoadTemporaryVariable.
	self assert: cfg b2 instructions first variableName equals: secondStoreName.
	self assert: cfg b2 instructions second operand1 equals: cfg b2 instructions first.
	
	self assert: cfg b3 instructions first isLoadTemporaryVariable.
	self assert: cfg b3 instructions first variableName equals: firstStoreName.
	self assert: cfg b3 instructions second operand1 equals: cfg b3 instructions first.
	
]
