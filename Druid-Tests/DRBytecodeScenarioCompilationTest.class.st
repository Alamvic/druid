Class {
	#name : 'DRBytecodeScenarioCompilationTest',
	#superclass : 'DRBytecodeCompilationTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedPops [

	self
		compileBytecode: 0
		selector: #bytecodePopOnTwoBranches
		thenDo: [ :generator | "Push the receiver"
			self initializeSStack: 0.
			"Then execute the druid's compiled code"
			generator value.
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedPushOnBranchOne [

	self
		compileBytecode: 0
		selector: #bytecodePushOnTwoBranches
		thenDo: [ :generator | 
			self initializeSStack: 0.
			generator value. "Call generator"
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17.

	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedPushOnBranchTwo [

	self
		compileBytecode: 0
		selector: #bytecodePushOnTwoBranches
		thenDo: [ :generator | 
			self initializeSStack: 0.
			generator value. "Call generator"
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: -17.

	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedTwoPops [

	self
		compileBytecode: 0
		selector: #bytecodeTwoPopOnTwoBranches
		thenDo: [ :generator | 
			self initializeSStack: 1.
			generator value. "Call generator"
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17 withArguments: { 42 }.

	self assert: machineSimulator receiverRegisterValue equals: 59
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedTwoPushesOnBranchOne [

	self
		compileBytecode: 0
		selector: #bytecodeTwoPushOnTwoBranches
		thenDo: [ :generator | 
			self initializeSStack: 0.
			generator value. "Call generator"
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17.

	self assert: machineSimulator receiverRegisterValue equals: 3
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedTwoPushesOnBranchTwo [

	self
		compileBytecode: 0
		selector: #bytecodeTwoPushOnTwoBranches
		thenDo: [ :generator | 
			self initializeSStack: 0.
			generator value. "Call generator"
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: -17.

	self assert: machineSimulator receiverRegisterValue equals: 7
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testFlushStackOnBranch1 [

	| mustBeBooleanTrampolineAddress |	
	self isSimpleStack ifTrue: [ self skip ].
	
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.

	self
		compileBytecode: 0
		selector: #bytecodeWithFlushStackOnDominator
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit ssPushConstant: 0.
			cogit bytecodePC: 237.
			generator value ].

	self prepareStackForPrimitiveReceiver: 0 arguments: #(  ) method: 0.

	self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress.
	
	self assert: cogit ssTop spilled.
	self assert: cogit ssTop constant equals: 0
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testFlushStackOnBranch2 [

	| deoptimizeAddress |
	self isSimpleStack ifTrue: [ self skip ].

	deoptimizeAddress := self compile: [ cogit RetN: 0 ].
	cogit ceDeoptimiseFrameTrampoline: deoptimizeAddress.

	cogit ceSendMustBeBooleanTrampoline: fakeTrampoline.

	self
		compileBytecode: 0
		selector: #bytecodeWithFlushStackOnDominator
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit ssPushConstant: 42.
			cogit bytecodePC: 237.
			generator value ].

	self prepareStackForPrimitiveReceiver: 0 arguments: #(  ) method: 0.

	self runFrom: cogInitialAddress until: deoptimizeAddress.
	
	self assert: cogit ssTop spilled.
	self assert: cogit ssTop constant equals: 42
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testFlushStackThenPop [

	self isSimpleStack ifTrue: [ self skip ].

	self
		compileBytecode: 0
		selector: #bytecodeWithPop
		thenDo: [ :generator |
			cogit ssPushConstant: 237.
			cogit ssPushConstant: 42.
			cogit ssFlushStack.
			generator value.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: 0.

	self assert: cogit ssTop constant equals: 237
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testPushTrueAndJumpFalse [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push true"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 77
		selector: #pushConstantTrueBytecode
		andBytecode: 184
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator |
			cogit bytecodePC: 16.
			cogit methodObj: method.
			cogit debugStackPointers:
				(cogit coInterpreter debugStackPointersFor: method).

			"Execute the druid's compiled code"
			cogit bytecodePC: cogit bytecodePC + 1. "Generated by optimisation preamble" 
			generator value.

			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.


			cogit bytecodePC: 19.
			cogit
				patchFixupTargetIfNeeded: (cogit fixupAt: 19)
				nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 42.
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory nilObject.
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testPushTrueAndJumpTrue [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push true"
			184 "jump if true over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 77
		selector: #pushConstantTrueBytecode
		andBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator |
			cogit bytecodePC: 16.
			cogit methodObj: method.
			cogit debugStackPointers:
				(cogit coInterpreter debugStackPointersFor: method).

			"Execute the druid's compiled code"
			cogit bytecodePC: cogit bytecodePC + 1. "Generated by optimisation preamble" 
			generator value.

			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.


			cogit bytecodePC: 19.
			cogit
				patchFixupTargetIfNeeded: (cogit fixupAt: 19)
				nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 42.
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory nilObject.
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testPushTrueAndReturn [

	self
		compileBytecode: 77 selector: #pushConstantTrueBytecode
		andBytecode: 92 selector: #returnTopFromMethod
		thenDo: [ :generator | generator value ].

	self executePrimitiveWithReceiver: memory nilObject.
	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testSuperBytecodePrimIdenticalAndJumpFalse [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "fake"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	"Force failing test if tries to compile the trampoline call:
		Should be removed due to super-instruction."
	cogit ceSendMustBeBooleanTrampoline: fakeTrampoline.

	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		andBytecode: 184
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator |
			self initializeSStack: 1.
		
			cogit bytecodePC: 16.
			cogit methodObj: method.
			cogit debugStackPointers:
				(cogit coInterpreter debugStackPointersFor: method).

			"Execute the druid's compiled code"
			cogit bytecodePC: cogit bytecodePC + 1. "Generated by optimisation preamble" 
			generator value.

			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.


			cogit bytecodePC: 19.
			cogit
				patchFixupTargetIfNeeded: (cogit fixupAt: 19)
				nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 42.
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory nilObject withArgument: memory nilObject.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testSuperBytecodePrimIdenticalAndJumpFalse2 [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "fake"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	"Force failing test if tries to compile the trampoline call:
		Should be removed due to super-instruction."
	cogit ceSendMustBeBooleanTrampoline: fakeTrampoline.

	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		andBytecode: 184
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator |
			self initializeSStack: 1.

			cogit bytecodePC: 16.
			cogit methodObj: method.
			cogit debugStackPointers:
				(cogit coInterpreter debugStackPointersFor: method).

			"Execute the druid's compiled code"
			cogit bytecodePC: cogit bytecodePC + 1. "Generated by optimisation preamble" 
			generator value.

			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.


			cogit bytecodePC: 19.
			cogit
				patchFixupTargetIfNeeded: (cogit fixupAt: 19)
				nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 42.
			cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory nilObject withArgument: memory trueObject.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testSuperInstructionCompilation [

	| compiledMethod fixupCheck returns nextBytecodeCheck |
	self
		compileBytecode: 77 selector: #fakePushConstantTrueBytecode
		options: #( superInstructions )
		thenDo: [ :generator | "Nothing" ].
	
	compiledMethod := compilerClass  >> #gen_fakePushConstantTrueBytecode.
	
	"Avoid super-instruction if there is a fixup to the next bytecode"
	fixupCheck := compiledMethod ast allChildren detect: [ :n | n isMessage and: [ n selector = #ifTrue: ] ].
	self assert: fixupCheck receiver receiver name equals: #nextFixup.
	self assert: fixupCheck receiver selector equals: #notAFixup.

	"Check for next bytecode"
	nextBytecodeCheck := fixupCheck arguments first allChildren detect: [ :n | n isMessage and: [ n selector = #ifTrue: ] ].
	self assert: nextBytecodeCheck receiver receiver name equals: #nextBytecode.
	self assert: nextBytecodeCheck receiver arguments first value equals: 92.
	
	"Two returns: super-instruction or compiler result"
	returns := compiledMethod ast allChildren select: [ :n | n isReturn ].
	self assert: returns size equals: 2.
	self assert: returns first value selector equals: #gen_pushConstantTrueBytecode_returnTopFromMethod_1.
	self assert: returns second value value equals: 0


]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testUnknownBytecode [

	| method deoptimizeTrampoline |
	deoptimizeTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceDeoptimiseFrameTrampoline: deoptimizeTrampoline.
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 0
		selector: #bytecodeWithDeoptimisation
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit bytecodePC: 237.
			generator value ].

	self prepareStackForPrimitiveReceiver: 0 arguments: #(  ) method: 0.

	"Should arrive to trampoline to put the object in the remembered set"
	self runFrom: cogInitialAddress until: deoptimizeTrampoline.
	self assert: machineSimulator receiverRegisterValue equals: 237
]
