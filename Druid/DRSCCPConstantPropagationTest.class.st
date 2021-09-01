Class {
	#name : #DRSCCPConstantPropagationTest,
	#superclass : #DRSCCPTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateBetweenBasicBlocks [

	| cfg copy1 |
	cfg := self setUpCFG: 3. 

	"T1 := 1"
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 1).
	
	"T2 := T1"
	cfg b2 addInstruction: (instructionFactory copy: copy1).
	
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: 1 asDRValue
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateToAddOperand [

	| cfg copy1 |
	cfg := self setUpCFG: 2.
		
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
	cfg := self setUpCFG: 2. 

	"T1 := 1"
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 1).
	"T2 := T1"
	cfg b1 addInstruction: (instructionFactory copy: copy1).
	
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions allButLast last operand1 equals: 1 asDRValue
]
