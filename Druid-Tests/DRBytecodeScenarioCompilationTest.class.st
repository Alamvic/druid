Class {
	#name : 'DRBytecodeScenarioCompilationTest',
	#superclass : 'DRBytecodeCompilationTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'accessing' }
DRBytecodeScenarioCompilationTest class >> bytecodes [

	 ^ {
		   { (0 -> #bytecodePopOnTwoBranches) }.
		   { (0 -> #bytecodePushOnTwoBranches) }.
		   { (0 -> #bytecodeTwoPopOnTwoBranches) }.
		   { (0 -> #bytecodeTwoPushOnTwoBranches) }.
		   { (0 -> #bytecodeWithFlushStackOnDominator) }.
		   { (0 -> #bytecodeWithPop) }.
		   { (0 -> #bytecodeWithDeoptimisation) }.
		   { (77 -> #fakePushConstantTrueBytecode) }.
		   {
			   (77 -> #pushConstantTrueBytecode).
			   (192 -> #shortConditionalJumpFalse) }.

		   {
			   (77 -> #pushConstantTrueBytecode).
			   (184 -> #shortConditionalJumpTrue) }.

		   {
			   (77 -> #pushConstantTrueBytecode).
			   (92 -> #returnTopFromMethod) }.
		   {
			   (118 -> #bytecodePrimIdenticalSistaV1).
			   (192 -> #shortConditionalJumpFalse) } }
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedPops [

	self compileBytecode: [
		self initializeSStack: 0.
		self compileBytecodeSelector: #bytecodePopOnTwoBranches.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedPushOnBranchOne [

	self compileBytecode: [
		self initializeSStack: 0.
		self compileBytecodeSelector: #bytecodePushOnTwoBranches.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17.

	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedPushOnBranchTwo [

	self compileBytecode: [
		self initializeSStack: 0.
		self compileBytecodeSelector: #bytecodePushOnTwoBranches.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: -17.

	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedTwoPops [

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodeTwoPopOnTwoBranches.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17 withArguments: { 42 }.

	self assert: machineSimulator receiverRegisterValue equals: 59
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedTwoPushesOnBranchOne [

	self compileBytecode: [
		self initializeSStack: 0.
		self compileBytecodeSelector: #bytecodeTwoPushOnTwoBranches.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: 17.

	self assert: machineSimulator receiverRegisterValue equals: 3
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testCoallescedTwoPushesOnBranchTwo [

	self compileBytecode: [
		self initializeSStack: 0.
		self compileBytecodeSelector: #bytecodeTwoPushOnTwoBranches.
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

	cogit bytecodePC: 237.
	self compileBytecode: [
		cogit ssPushConstant: 0.
		self compileBytecodeSelector: #bytecodeWithFlushStackOnDominator ].

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

	cogit bytecodePC: 237.
	self compileBytecode: [
		cogit ssPushConstant: 42.
		self compileBytecodeSelector: #bytecodeWithFlushStackOnDominator ].


	self prepareStackForPrimitiveReceiver: 0 arguments: #(  ) method: 0.

	self runFrom: cogInitialAddress until: deoptimizeAddress.

	self assert: cogit ssTop spilled.
	self assert: cogit ssTop constant equals: 42
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testFlushStackThenPop [

	self isSimpleStack ifTrue: [ self skip ].

	self compileBytecode: [
		cogit ssPushConstant: 237.
		cogit ssPushConstant: 42.
		cogit ssFlushStack.
		self compileBytecodeSelector: #bytecodeWithPop.
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
		          bytecodes: #[ 77 192 90 91 ];
		          numberOfTemporaries: 2;
		          buildMethod. "push true" "jump if false over next" "return false" "return true"

	cogit methodObj: method.
	cogit bytecodePC: 16.
	cogit debugStackPointers:
		(cogit coInterpreter debugStackPointersFor: method).

	self compileBytecode: [
		cogit bytecodePC: 17. "Generated by optimisation preamble"
		self
			compileBytecodeSelector: #pushConstantTrueBytecode
			andSelector: #shortConditionalJumpFalse.

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
		          bytecodes: #[ 77 184 90 91 ];
		          numberOfTemporaries: 2;
		          buildMethod. "push true" "jump if true over next" "return false" "return true"

	cogit methodObj: method.
	cogit bytecodePC: 16.
	cogit debugStackPointers:
		(cogit coInterpreter debugStackPointersFor: method).

	self compileBytecode: [
		cogit bytecodePC: 17. "Generated by optimisation preamble"
		self
			compileBytecodeSelector: #pushConstantTrueBytecode
			andSelector: #shortConditionalJumpTrue.

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

	self compileBytecode: [
		self
			compileBytecodeSelector: #pushConstantTrueBytecode
			andSelector: #returnTopFromMethod ].

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
		          bytecodes: #[ 77 192 90 91 ];
		          numberOfTemporaries: 2;
		          buildMethod. "fake" "jump if false over next" "return false" "return true"

	"Force failing test if tries to compile the trampoline call:
		Should be removed due to super-instruction."
	cogit ceSendMustBeBooleanTrampoline: fakeTrampoline.

	cogit methodObj: method.
	cogit bytecodePC: 16.
	cogit debugStackPointers:
		(cogit coInterpreter debugStackPointersFor: method).

	self compileBytecode: [
		self initializeSStack: 1.

		cogit bytecodePC: 17.
		self
			compileBytecodeSelector: #bytecodePrimIdenticalSistaV1
			andSelector: #shortConditionalJumpFalse.

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

	self
		executePrimitiveWithReceiver: memory nilObject
		withArgument: memory nilObject.

	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testSuperBytecodePrimIdenticalAndJumpFalse2 [

	| method |
	method := methodBuilder
		          newMethod;
		          literals: { (memory integerObjectOf: 42) };
		          bytecodes: #[ 77 192 90 91 ];
		          numberOfTemporaries: 2;
		          buildMethod. "fake" "jump if false over next" "return false" "return true"

	"Force failing test if tries to compile the trampoline call:
		Should be removed due to super-instruction."
	cogit ceSendMustBeBooleanTrampoline: fakeTrampoline.

	cogit methodObj: method.
	cogit bytecodePC: 16.
	cogit debugStackPointers:
		(cogit coInterpreter debugStackPointersFor: method).

	self compileBytecode: [
		self initializeSStack: 1.

		cogit bytecodePC: 17.
		self
			compileBytecodeSelector: #bytecodePrimIdenticalSistaV1
			andSelector: #shortConditionalJumpFalse.

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

	self
		executePrimitiveWithReceiver: memory nilObject
		withArgument: memory trueObject.

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRBytecodeScenarioCompilationTest >> testSuperInstructionCompilation [

	| compiledMethod fixupCheck returns nextBytecodeCheck |
	
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
	cogit bytecodePC: 237.

	self compileBytecode: [
		self compileBytecodeSelector: #bytecodeWithDeoptimisation ].

	self prepareStackForPrimitiveReceiver: 0 arguments: #(  ) method: 0.

	"Should arrive to trampoline to put the object in the remembered set"
	self runFrom: cogInitialAddress until: deoptimizeTrampoline.
	self assert: machineSimulator receiverRegisterValue equals: 237
]
