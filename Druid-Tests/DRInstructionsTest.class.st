Class {
	#name : 'DRInstructionsTest',
	#superclass : 'DRIRTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'tests' }
DRInstructionsTest >> setUp [

	super setUp.
	compilerCompiler := DRMethodCompilerCompiler new
]

{ #category : 'tests' }
DRInstructionsTest >> testReplaceInstructionUpdateOperandAndUsers [

	| cfg instruction user newInstruction |
	cfg := self setUpCFG: 1.
	instruction := cfg b1 copy: 1.
	user := cfg b1 copy: instruction.
	cfg validate.

	self assert: user operand equals: instruction.
	self assert: instruction users unique equals: user.
	
	newInstruction := cfg instructionFactory copy: 2.
	instruction replaceBy: newInstruction.
	
	self assert: instruction users isEmpty.
	self assert: user operand equals: newInstruction.
	self assert: newInstruction users unique equals: user

]

{ #category : 'tests' }
DRInstructionsTest >> testReplaceMessageSendUpdateArguments [

	| cfg instruction message newInstruction |
	cfg := self generateDruidIRFor: #basicInlineMethod.
	cfg applyOptimisation: DRCleanControlFlow new.
	instruction := cfg firstBasicBlock instructions second.
	message := instruction users unique.

	self assert: message isMessageSend.

	newInstruction := cfg instructionFactory copy: 2.
	instruction replaceBy: newInstruction.

	self assert: message arguments first equals: newInstruction
]

{ #category : 'tests' }
DRInstructionsTest >> testReplaceMessageSendUpdateInnerDependencies [

	| cfg instruction message newInstruction |
	cfg := self generateDruidIRFor: #simpleMethodWithBlockReadingOuterVariable.
	cfg applyOptimisation: DRCleanControlFlow new.
	instruction := cfg firstBasicBlock instructions third.
	message := cfg firstBasicBlock instructions fourth.
	 
	self assert: (instruction users includes: message).

	self assert: message isMessageSend.
	self assert: message innerDependencies unique equals: instruction.

	newInstruction := cfg instructionFactory copy: 2.
	instruction replaceBy: newInstruction.

	self assert: message innerDependencies unique equals: newInstruction.
	self assert: (newInstruction users includes: message).
	self assert: instruction users isEmpty
]
