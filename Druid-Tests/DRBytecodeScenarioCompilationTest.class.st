Class {
	#name : #DRBytecodeScenarioCompilationTest,
	#superclass : #DRBytecodeCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRBytecodeScenarioCompilationTest >> testFlushStackOnBranch1 [

	| mustBeBooleanTrampolineAddress |
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

{ #category : #tests }
DRBytecodeScenarioCompilationTest >> testFlushStackOnBranch2 [

	| deoptimizeAddress |
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

{ #category : #tests }
DRBytecodeScenarioCompilationTest >> testPopAfterFlushStack [

	self
		compileBytecode: 0
		selector: #bytecodeWithPop
		thenDo: [ :generator |
			cogit ssPushConstant: 237.
			cogit ssPushConstant: 42.
			cogit ssFlushStack.
			generator value.
			cogit genUpArrowReturn ].

	self executeMethodWithReceiver: 0 withArguments: #(  ).

	self assert: cogit ssTop constant equals: 237
]

{ #category : #tests }
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

{ #category : #tests }
DRBytecodeScenarioCompilationTest >> testSuperInstructionCompilation [

	| compiledMethod check returns |
	self
		compileBytecode: 77 selector: #fakePushConstantTrueBytecode
		thenDo: [ :generator | "Nothing" ].
	
	compiledMethod := DruidTestRTLCompiler >> #gen_fakePushConstantTrueBytecode.
	
	check := compiledMethod ast allChildren detect: [ :n | n isMessage and: [ n selector = #ifTrue: ] ].
	self assert: check receiver receiver name equals: #nextBytecode.
	self assert: check receiver arguments first value equals: 92.
	
	returns := compiledMethod ast allChildren select: [ :n | n isReturn ].
	self assert: returns size equals: 2.
	self assert: returns first value selector equals:  #gen_pushConstantTrueBytecode_returnTopFromMethod


]

{ #category : #tests }
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
