Class {
	#name : 'DRInstructionsTest',
	#superclass : 'DRIRTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'test' }
DRInstructionsTest >> inputClass [

	^ DROpalExamples 
]

{ #category : 'tests' }
DRInstructionsTest >> setUp [

	super setUp.
	compilerCompiler := DRMethodCompilerCompiler new
]

{ #category : 'tests' }
DRInstructionsTest >> testCreateBlockClosureUsesTemporaries [

	| cfg instruction block |
	cfg := self generateDruidIRFor: #simpleMethodWithBlockReadingOuterVariable.
	cfg applyOptimisation: DRCleanControlFlow new.
	instruction := cfg firstBasicBlock instructions fourth.
	block := cfg firstBasicBlock instructions sixth.

	self assert: block isDRBlockClosure.
	self assert: (instruction users includes: block).
	self assert: block dependencies unique equals: instruction
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
	
	"Take the third instruction, which is the first argument"
	instruction := cfg firstBasicBlock instructions third.
	message := instruction users unique.
	self assert: message isMessageSend.
	self assert: message arguments first identicalTo: instruction.

	newInstruction := cfg instructionFactory copy: 2.
	instruction replaceBy: newInstruction.

	self assert: message arguments first equals: newInstruction
]
