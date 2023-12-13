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
