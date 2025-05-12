Class {
	#name : 'DRProductionBytecodeTest',
	#superclass : 'DRBytecodeCompilationTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'accessing' }
DRProductionBytecodeTest class >> bytecodes [

	 ^ {
		   {
			   (0 -> #pushReceiverVariableBytecode).
			   #( version0 ) }.
		   {
			   (1 -> #pushReceiverVariableBytecode).
			   #( version1 ) }.
		   {
			   (16 -> #pushLiteralVariable16CasesBytecode).
			   #( version0 ) }.
		   {
			   (16 + 1 -> #pushLiteralVariable16CasesBytecode).
			   #( version1 ) }.
		   {
			   (16 + 15 -> #pushLiteralVariable16CasesBytecode).
			   #( version15 ) }.
		   { (32 -> #pushLiteralConstantBytecode) }.
		   {
			   (33 -> #pushLiteralConstantBytecode).
			   #( version1 ) }.
		   { (64 -> #pushTemporaryVariableBytecode) }.
		   { (76 -> #pushReceiverBytecode) }.
		   { (77 -> #pushConstantTrueBytecode) }.
		   { (80 -> #pushConstantZeroBytecode) }.
		   { (81 -> #pushConstantOneBytecode) }.
		   { (83 -> #duplicateTopBytecode) }.
		   { (88 -> #returnReceiver) }.
		   { (89 -> #returnTrue) }.
		   { (90 -> #returnFalse) }.
		   { (91 -> #returnNil) }.
		   { (92 -> #returnTopFromMethod) }.
		   {
			   (96 -> #bytecodePrimAdd).
			   #( staticTypePrediction ) }.
		   {
			   (96 -> #bytecodePrimAdd).
			   #( versionSend ) }.
		   { (118 -> #bytecodePrimIdenticalSistaV1) }.
		   { (120 -> #bytecodePrimNotIdenticalSistaV1) }.
		   {
			   (128 -> #sendLiteralSelector0ArgsBytecode).
			   #( version0 ) }.
		   {
			   (128 + 1 -> #sendLiteralSelector0ArgsBytecode).
			   #( version1 ) }.
		   { (176 -> #shortUnconditionalJump) }.
		   { (184 -> #shortConditionalJumpTrue) }.
		   { (192 -> #shortConditionalJumpFalse) }.
		   {
			   (200 -> #storeAndPopReceiverVariableBytecode).
			   #( version0 ) }.
		   {
			   (200 + 7 -> #storeAndPopReceiverVariableBytecode).
			   #( version7 ) }.
		   { (225 -> #extBBytecode) }.
		   { (231 -> #pushNewArrayBytecode) }.
		   { (232 -> #extPushIntegerBytecode) }.
		   { (235 -> #extSendSuperBytecode) }.
		   { (237 -> #extUnconditionalJump) }.
		   { (240 -> #extStoreAndPopReceiverVariableBytecode) }.
		   { (243 -> #extStoreReceiverVariableBytecode) }.
		   { (244 -> #extStoreLiteralVariableBytecode) }.
		   { (249 -> #extPushFullClosureBytecode) }.
		   { (253 -> #storeAndPopRemoteTempLongBytecode) }.
		   { (999 -> #extendedPushBytecode) } }
]

{ #category : 'tests' }
DRProductionBytecodeTest >> setUp [ 

	| method |
	super setUp.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.
	
	"Set a random methodObj and bytecodePC to avoid nil errors"
	cogit methodObj: method.
	cogit bytecodePC: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeAddOneSpilledArg [
	 "We need the send trampoline to compile even though it is not called"
	 cogit ordinarySendTrampolineAt: 1 put: fakeTrampoline.

	 cogit methodOrBlockNumArgs: 0.
	 cogit methodOrBlockNumTemps: 0.
	 cogit initSimStackForFramefulMethod: 0.

	 self compileBytecode: [
		 cogit ssPushRegister: ReceiverResultReg.
		 cogit ssFlushStack.
		 cogit ssPushRegister: Arg0Reg.

		 self compileBytecodeSelector: #bytecodePrimAdd.
		 cogit genReturnTopFromMethod ].

	 self
		 executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		 withArguments: { (memory integerObjectOf: 42) }.
	 self
		 assert:
		 (memory integerValueOf: machineSimulator receiverRegisterValue)
		 equals: 42 + 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeAddOverflow [
	 "In case of overflow, this routine is called and returns the receiver unchanged"
	 sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	 cogit ordinarySendTrampolineAt: 1 put: sendTrampolineAddress.

	 self compileBytecode: [
		 self initializeSStack: 1.
		 self compileBytecodeSelector: #bytecodePrimAdd.
		 cogit Stop ].

	 "Should run, overflow, jump to the trampoline and leave the registers as they are"
	 self
		 prepareStackForPrimitiveReceiver: (memory integerObjectOf: memory maxSmallInteger)
		 arguments: { (memory integerObjectOf: 1) }
		 method: memory nilObject.

	 "Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	 self runFrom: cogInitialAddress until: sendTrampolineAddress.

	 self
		 assert: (memory integerValueOf: machineSimulator receiverRegisterValue)
		 equals: memory maxSmallInteger
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeAddSendMessage [
	"Setup the send trampoline"

	| method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 1 put: sendTrampolineAddress. "arg"

	method := methodBuilder
		          newMethod;
		          literals: { (selector := memory integerObjectOf: 42) };
		          buildMethod.


	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimAdd version: 'Send'.
		cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: { memory integerObjectOf: 42 }
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: cogInitialAddress until: sendTrampolineAddress.

	"First argument: the selector"
	self
		assert: machineSimulator classRegisterValue
		equals: (-1 twoComplementOfBitSize: 64). "+ selector index"
	"Third argument: the receiver, then the argument is on the stack"
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 17).
	self
		assert: machineSimulator arg0RegisterValue
		equals: (memory integerObjectOf: 42)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeAddTypePredicted [

	"We need the send trampoline to compile even though it is not called"
	cogit ordinarySendTrampolineAt: 1 put: fakeTrampoline.

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimAdd.
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 42) }.
	self
		assert:
		(memory integerValueOf: machineSimulator receiverRegisterValue)
		equals: 42 + 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeAddWithSpilledArgs [
	 "We need the send trampoline to compile even though it is not called"
	 cogit ordinarySendTrampolineAt: 1 put: fakeTrampoline.

	 cogit methodOrBlockNumArgs: 0.
	 cogit methodOrBlockNumTemps: 0.
	 cogit initSimStackForFramefulMethod: 0.

	 self compileBytecode: [
		 cogit ssPushRegister: ReceiverResultReg.
		 cogit ssPushRegister: Arg0Reg.
		 cogit ssFlushStack.

		 self compileBytecodeSelector: #bytecodePrimAdd.
		 cogit genReturnTopFromMethod ].

	 self
		 executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		 withArguments: { (memory integerObjectOf: 42) }.
	 self
		 assert:
		 (memory integerValueOf: machineSimulator receiverRegisterValue)
		 equals: 42 + 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeDuplicateTop [

	 self compileBytecode: [
		 cogit ssPushRegister: TempReg.
		 self compileBytecodeSelector: #duplicateTopBytecode.
		 cogit genReturnTopFromMethod ].

	 machineSimulator temporaryRegisterValue: 17.
	 self executePrimitiveWithReceiver: (memory integerObjectOf: 42).
	 self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeExtPushInteger [

	cogit extB: 1. "256"
	cogit byte1: 13.

	self compileBytecode: [
		self compileBytecodeSelector: #extPushIntegerBytecode.
		cogit genReturnTopFromMethod ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 256 + 13).

	self assert: cogit extB equals: 0
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeExtendedPushBytecode0 [

	| method variableType variableIndex object |
	method := methodBuilder newMethod buildMethod.
	cogit methodObj: method.

	variableType := 0. "PushReceiverVariable"
	variableIndex := 0.

	cogit byte1: variableType << 6 + variableIndex.

	self compileBytecode: [
		self compileBytecodeSelector: #extendedPushBytecode.
		cogit genReturnTopFromMethod ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executeMethodWithReceiver: object withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeExtendedPushBytecode1 [

	| method variableType variableIndex |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 0;
		          buildMethod.
	cogit methodObj: method.

	variableType := 1. "PushTemporaryVariable"
	variableIndex := 0.

	cogit byte1: variableType << 6 + variableIndex.

	cogit methodOrBlockNumArgs: 1.
	cogit methodOrBlockNumTemps: 1.
	cogit initSimStackForFramefulMethod: 2.

	self compileBytecode: [
		self compileBytecodeSelector: #extendedPushBytecode.
		cogit genReturnTopFromMethod ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeExtendedPushBytecode2 [

	| method variableType variableIndex |
	method := methodBuilder newMethod
		          literals: { 42 };
		          buildMethod.
	cogit methodObj: method.

	variableType := 2. "PushLiteralIndex"
	variableIndex := 0.

	cogit byte1: variableType << 6 + variableIndex.

	self compileBytecode: [
		self compileBytecodeSelector: #extendedPushBytecode.
		cogit genReturnTopFromMethod ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeExtendedSuperSend [

	| method numArgs |
	selectorIndex := 1.
	numArgs := 1.
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit superSendTrampolineAt: numArgs put: sendTrampolineAddress.

	cogit byte1: selectorIndex << 3 + numArgs.

	method := methodBuilder
		          newMethod;
		          numberOfArguments: numArgs;
		          literals: {
				          (memory integerObjectOf: 41).
				          (memory integerObjectOf: 42).
				          (memory integerObjectOf: 43) };
		          buildMethod.


	cogit methodObj: method.
	cogit methodOrBlockNumArgs: 1.
	cogit methodOrBlockNumTemps: 1.
	cogit initSimStackForFramefulMethod: 0 "fake Bytecode pc".

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #extSendSuperBytecode.
		cogit Stop ].

	"Execute the method with receiver smi 17 and argument smi 53"
	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: { (memory integerObjectOf: 53) }
		method: method.

	self runFrom: cogInitialAddress until: sendTrampolineAddress.

	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: selectorIndex.
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17).
	self assert: machineSimulator arg0RegisterValue equals: (memory integerObjectOf: 53).
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeExtendedUnconditionalJump [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 176 90 "return false" 91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.
		
	cogit ceCheckForInterruptTrampoline: fakeTrampoline.
		
	cogit methodObj: method.
	cogit byte1: 0.
	cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

	self compileBytecode: [
		cogit bytecodePC: 16.
		self compileBytecodeSelector: #extUnconditionalJump.
		self assert: cogit deadCode.
		
		"We will jump over the stop!"
		cogit bytecodePC: 17.
		cogit genReturnFalse.
		cogit Stop.

		cogit bytecodePC: 18.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 18) nextOpcodeIndex: cogit opcodeIndex.
		cogit genReturnTrue. ].

	self executePrimitiveWithReceiver: (memory integerObjectOf: 17).

	self assert: machineSimulator receiverRegisterValue equals: memory trueObject.
	
	self flag: #TODO. "Force bytecode annotation on staged branches!"
	self assert: (cogit hasAnnotatedAbstractInstructions: cogit getIsBytecodePCReference)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeExtendedUnconditionalJumpBackward [

	| method bytecodeAddress |
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 91 "return true" 176 90 "return false"];
		numberOfTemporaries: 2;
		buildMethod.
		
	cogit ceCheckForInterruptTrampoline: fakeTrampoline.

	cogit methodObj: method.
	cogit byte1: -3.
	cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

	self compileBytecode: [
		cogit initializeFixupAt: 16.
		cogit bytecodePC: 16.
		cogit mergeWithFixupIfRequired: (cogit fixupAt: 16).
		cogit genReturnTrue.

		bytecodeAddress := cogit Label.
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #extUnconditionalJump.
		self assert: cogit deadCode.

		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit genReturnFalse.
		cogit Stop ].
	
	cogInitialAddress := bytecodeAddress address.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 17).

	self assert: machineSimulator receiverRegisterValue equals: memory trueObject.
	
	self assert: (cogit hasAnnotatedAbstractInstructions: cogit getIsBytecodePCReference)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfDifferentIntegers [

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 42) }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfEqualIntegers [

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 17) }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjects [

	| oop |
	oop := self newObjectWithSlots: 0.

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: oop withArguments: { oop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjectsWithBothForwarders [

	| oop forwardedOop oop2 forwardedOop2 |	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	forwardedOop2 := self newObjectWithSlots: 0.
	oop2 := memory pinObject: forwardedOop2.
	self assert: (memory isForwarded: forwardedOop2).

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: forwardedOop2 withArguments: { forwardedOop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjectsWithForwarderArgument [

	| oop forwardedOop oop2 |
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.
	
	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: oop2 withArguments: { forwardedOop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop |
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfNonIdenticalObjects [

	| oop oop2 |
	oop := self newObjectWithSlots: 0.
	oop2 := self newObjectWithSlots: 0.

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: oop withArguments: { oop2 }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfNonIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop oop2 |	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop2 }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNonIdenticalOfDifferentIntegers [

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 42) }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfEqualIntegers [

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 17) }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjects [

	| oop |
	oop := self newObjectWithSlots: 0.
	
	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: oop withArguments: { oop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjectsWithBothForwarders [

	| oop forwardedOop forwardedOop2 oop2 |
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	forwardedOop2 := self newObjectWithSlots: 0.
	oop2 := memory pinObject: forwardedOop2.
	self assert: (memory isForwarded: forwardedOop2).
	
	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: forwardedOop2 withArguments: { forwardedOop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjectsWithForwarderArgument [

	| oop forwardedOop oop2 |
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.
	
	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: oop2 withArguments: { forwardedOop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop |
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfNonIdenticalObjects [

	| oop oop2 |
	oop := self newObjectWithSlots: 0.
	oop2 := self newObjectWithSlots: 0.

	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: oop withArguments: { oop2 }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfNonIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop oop2 |
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.
	
	self compileBytecode: [
		self initializeSStack: 1.
		self compileBytecodeSelector: #bytecodePrimNotIdenticalSistaV1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop2 }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushConstantOneBytecode [

	self compileBytecode: [
		self compileBytecodeSelector: #pushConstantOneBytecode.
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: memory nilObject
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 1)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushConstantZeroBytecode [

	self compileBytecode: [
		self compileBytecodeSelector: #pushConstantZeroBytecode.
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: memory nilObject
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 0)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant [

	| method |
	method := methodBuilder newMethod
		          literals: { (memory integerObjectOf: 42) };
		          buildMethod.

	cogit methodObj: method.

	self compileBytecode: [
		self compileBytecodeSelector: #pushLiteralConstantBytecode. "0 based"
		cogit genReturnTopFromMethod ].

	self
		executePrimitiveWithReceiver: memory nilObject
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 42).

	self assert:
		(cogit hasAnnotatedAbstractInstructions: cogit getIsObjectReference)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant2 [

	| method |

	method := methodBuilder
		          newMethod
		          literals: { (memory integerObjectOf: 42). (memory integerObjectOf: 43) };
		          buildMethod.
		
	cogit methodObj: method.

	self compileBytecode: [
		self compileBytecodeSelector: #pushLiteralConstantBytecode version: 1. "index = 1 (0 based)"
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory nilObject
		withArguments: #().
		
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 43).
	
	self assert:
		(cogit hasAnnotatedAbstractInstructions: cogit getIsObjectReference)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable0 [

	self testBytecodePushLiteralVariable: 0
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable1 [

	self testBytecodePushLiteralVariable: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable15 [

	self testBytecodePushLiteralVariable: 15
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable: tempIndex [

	| method |
	method := methodBuilder newMethod
		          literals:
			          (((1 to: 16) collect: [ :e | nil -> (e + 16rFF - 1) ])
				           collect: [ :e | e forMemory: memory inMethod: nil ]);
		          buildMethod.

	cogit methodObj: method.

	self compileBytecode: [
		self compileBytecodeSelector: #pushLiteralVariable16CasesBytecode version: tempIndex.
		cogit genReturnTopFromMethod ].

	self
		executeMethodWithReceiver: memory nilObject
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: tempIndex + 16rFF).

	self assert:
		(cogit hasAnnotatedAbstractInstructions: cogit getIsObjectReference)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTop1AllocatedRegister [

	self testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTop2AllocatedRegister [

	self testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: 2
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTop3AllocatedRegister [

	self testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: 3
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: n [

	| method |
	method := methodBuilder newMethod
		          literals:
			          (((1 to: 16) collect: [ :e | nil -> (e + 16rFF - 1) ])
				           collect: [ :e | e forMemory: memory inMethod: nil ]);
		          buildMethod.

	cogit methodObj: method.

	self compileBytecode: [
		"Push the top n elements to the stack"
		1 to: n do: [ :i | | r |
			r := cogit allocateRegNotConflictingWith: 0.
			cogit MoveCq: i R: r.
			cogit ssPushRegister: r.
		].

		self compileBytecodeSelector: #pushLiteralVariable16CasesBytecode version: 0.
		
		cogit ssFlushStack.

		"Pop the top n elements..."
		1 to: n do: [ :i |	cogit ssPopTopToReg: ReceiverResultReg ].
			
		"... the last should be the first pushed constant"
		cogit genReturnTopFromMethod ].

	self
		executeMethodWithReceiver: memory nilObject
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushNewArray [

	| size |
	size := 1.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit byte1: size.

	self compileBytecode: [
		self compileBytecodeSelector: #pushNewArrayBytecode.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory nilObject.
	
	self assert: (memory isArray: machineSimulator receiverRegisterValue).
	self assert: (memory numSlotsOf: machineSimulator receiverRegisterValue) equals: size.
	self assert: (memory fetchPointer: 0 ofObject: machineSimulator receiverRegisterValue) equals: memory nilObject
	
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushNewArrayBig [

	| size |
	size := 10.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit byte1: size.

	"Allocation touch the threshold"
	memory scavengeThreshold: memory freeStart + 1.

	self compileBytecode: [
		self compileBytecodeSelector: #pushNewArrayBytecode.
		cogit Stop ].

	"Deoptimize to run the scavenge"
	self runFrom: cogInitialAddress until: fakeTrampoline
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushReceiverDoesNotOverridePreviousPush [

	| method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 0;
		          buildMethod.

	cogit methodObj: method.

	cogit methodOrBlockNumArgs: 1.
	cogit methodOrBlockNumTemps: 1.
	cogit initSimStackForFramefulMethod: 2.

	self compileBytecode: [
		"Push temporary 0 -- aka first argument if there are arguments"
		self compileBytecodeSelector: #pushTemporaryVariableBytecode.
		
		"Push receiver, then pop it"
		self compileBytecodeSelector: #pushReceiverBytecode.
		cogit ssPopTopToReg: (cogit allocateRegNotConflictingWith: 0).
		
		cogit genReturnTopFromMethod ].

	self
		executeMethodWithReceiver: memory nilObject
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg0DoesNotOverridePreviousPush [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 0;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 0. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 1.
			cogit initSimStackForFramefulMethod: 2.

			cogit gen_pushReceiverBytecode.

			"Push the temp, then pop it"
			generator value.
			cogit ssPopTopToReg: (cogit allocateRegNotConflictingWith: 0).
	
			"Pop the value pushed by the first bytecode"
			cogit ssPopTopToReg: ReceiverResultReg.
		
			"Return the top of the stack which is the receiver"
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg0NoTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 0;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 0. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 1.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg0WithTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 1;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 0. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg1NoTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 2;
		          numberOfTemporaries: 0;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 2.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 43 ).

	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg1WithTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 2;
		          numberOfTemporaries: 2;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 2.
			cogit methodOrBlockNumTemps: 4.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 43 ).

	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp0NoArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 0;
		          numberOfTemporaries: 1;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 0. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 0.
			cogit methodOrBlockNumTemps: 1.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 42 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp0WithArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 1;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 5 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp1NoArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 0;
		          numberOfTemporaries: 2;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 0.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 42 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp1WithArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 2;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 2. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 3.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 5 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeReturnFalse [

	 | method |
	 method := methodBuilder newMethod buildMethod.

	 cogit methodObj: method.

	 self compileBytecode: [ self compileBytecodeSelector: #returnFalse ].

	 self
		 executePrimitiveWithReceiver: memory nilObject
		 withArguments: #(  ).

	 self
		 assert: machineSimulator receiverRegisterValue
		 equals: memory falseObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeReturnNil [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self compileBytecode: [ self compileBytecodeSelector: #returnNil ].

	self
		executePrimitiveWithReceiver: memory trueObject
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory nilObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeReturnTrue [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self compileBytecode: [ self compileBytecodeSelector: #returnTrue ].

	self
		executePrimitiveWithReceiver: memory nilObject
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeSendLiteral0ToSelf [

	"128 - sendLiteralSelector0ArgsBytecode selector=literal0, numargs=0.
	Test we do a send and we end up in a trampoline"
	| index method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 0 put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	cogit methodObj: method.

	index := 0.

	self compileBytecode: [ 
		self initializeSStack: 0.
		self compileBytecodeSelector: #sendLiteralSelector0ArgsBytecode version: index.
		cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: cogInitialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: index.
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeSendLiteral1ToSelf [

	"129 - sendLiteralSelector0ArgsBytecode selector=literal0, numargs=0.
	Test we do a send and we end up in a trampoline"
	
	"Setup the send trampoline"
	| index method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 0 put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			memory nilObject.
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	cogit methodObj: method.

	index := 1.

	self compileBytecode: [ 
		self initializeSStack: 0.
		self compileBytecodeSelector: #sendLiteralSelector0ArgsBytecode version: index. "0 based"
		cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: cogInitialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: index.
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfFalseWithFalse [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push false"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpFalse.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory falseObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfFalseWithNil [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push false"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpFalse.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].

	self
		prepareStackForSendReceiver: memory nilObject
		arguments: #()
		method: method.
	self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress.

	self assert: machineSimulator receiverRegisterValue equals: memory nilObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfFalseWithTrue [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push false"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpFalse.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].
		
	self executePrimitiveWithReceiver: memory trueObject.

	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfTrueWithFalse [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
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

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpTrue.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].

	self executePrimitiveWithReceiver: memory falseObject.

	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfTrueWithFalseNotMustBeBoolean [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
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

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpTrue.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].

	self
		prepareStackForSendReceiver: memory falseObject
		arguments: #()
		method: method.

	self should: [ self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress ] raise: Exception 
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfTrueWithNil [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
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

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpTrue.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].

	self
		prepareStackForSendReceiver: memory nilObject
		arguments: #()
		method: method.
	self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress.

	self assert: machineSimulator receiverRegisterValue equals: memory nilObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfTrueWithTrue [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
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

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpTrue.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].
	
	self executePrimitiveWithReceiver: memory trueObject.

	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortJumpIfTrueWithTrueNotMustBeBoolean [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
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

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

		cogit bytecodePC: 16.
		cogit ssPushRegister: ReceiverResultReg.
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 17.
		self compileBytecodeSelector: #shortConditionalJumpTrue.
		
		"We will jump over the stop!"
		cogit bytecodePC: 18.
		cogit ssPushConstant: 0.
		cogit genReturnTopFromMethod.
		cogit Stop.
		
		cogit bytecodePC: 19.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
		cogit ssPushConstant: 1.
		cogit genReturnTopFromMethod ].

	self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
		
	self should: [ self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress ] raise: Exception 
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testBytecodeShortUnconditionalJump [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 176 90 "return false" 91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	cogit methodObj: method.

	self compileBytecode: [ 
		cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).
		
		"Execute the druid's compiled code"
		cogit bytecodePC: 16.
		self compileBytecodeSelector: #shortUnconditionalJump.
		self assert: cogit deadCode.
		
		"We will jump over the stop!"
		cogit bytecodePC: 17.
		cogit genReturnFalse.
		cogit Stop.

		cogit bytecodePC: 18.
		cogit patchFixupTargetIfNeeded: (cogit fixupAt: 18) nextOpcodeIndex: cogit opcodeIndex.
		cogit genReturnTrue.
		cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: (memory integerObjectOf: 17).

	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtBBytecode [

	cogit byte1: 1.
	cogit extB: 7.

	self compileBytecodeSelector: #extBBytecode.

	self assert: cogit extB equals: 7 << 8 + 1.
	self assert: cogit numExtB equals: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtBBytecode2 [

	cogit byte1: 256.
	cogit extB: 7.

	self compileBytecodeSelector: #extBBytecode.

	self assert: cogit extB equals: 0.
	self assert: cogit numExtB equals: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtBBytecode3 [

	cogit byte1: 128.
	cogit extB: 7.

	self compileBytecodeSelector: #extBBytecode.

	self assert: cogit extB equals: -128.
	self assert: cogit numExtB equals: 1
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtPushFullClosure [
	"Fake test, hard to test now. Let's come back in the future."

	self skip.
	
	self compileBytecodeSelector: #extPushFullClosureBytecode.

	self flag: #TODO "Test the execution of compiled code"
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreAndPopReceiverVariableBytecode [

	| object index |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.
	cogit byte1: (index := 1).

	self compileBytecode: [ 
		cogit ssPushRegister: TempReg.
		self compileBytecodeSelector: #extStoreAndPopReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: index + 1.

	machineSimulator temporaryRegisterValue: memory trueObject.
	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: index ofObject: object) equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreLiteralVariableBytecode [

	| method literal |
	self createEphemeronClass.
	literal := self newEphemeronObject.
	method := methodBuilder newMethod
		          literals: { literal };
		          buildMethod.

	cogit methodObj: method.

	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		self initializeSStack: 0.
		self compileBytecodeSelector: #extStoreLiteralVariableBytecode.
		cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: memory trueObject.

	self assert: machineSimulator receiverRegisterValue equals: memory trueObject.
	
	self assert: (memory fetchPointer: 1 ofObject: literal) equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreLiteralVariableBytecodeWriteBarrier [

	| method literal newObj storeCheckTrampoline |
	self createEphemeronClass.
	literal := self newOldEphemeronObject.
	method := methodBuilder newMethod
		          literals: { literal };
		          buildMethod.

	newObj := self newObjectWithSlots: 0.

	cogit methodObj: method.

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		self initializeSStack: 0.
		self compileBytecodeSelector: #extStoreLiteralVariableBytecode.
		cogit genUpArrowReturn ].

	self
		prepareStackForPrimitiveReceiver: newObj
		arguments: #(  )
		method: method.

	"Should arrive to trampoline to put the object in the remembered set"
	self runFrom: cogInitialAddress until: storeCheckTrampoline.

	self assert: machineSimulator temporaryRegisterValue equals: literal.

	cogit backEnd hasLinkRegister
		ifTrue: [ "Push the link register -> a.k.a. caller"
		self assert: (self stackAddressAt: 0) equals: callerAddress ]
		ifFalse: [ "Push the return address -> a.k.a after the call (inside the compiled code)"
			self assert: ((self stackAddressAt: 0)
					 between: cogInitialAddress
					 and: cogInitialAddress + codeSize) ].

	self assert: (self stackAddressAt: 1) equals: newObj
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecode [

	| object |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.

	machineSimulator temporaryRegisterValue: memory trueObject.
	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self
		assert: (memory fetchPointer: 0 ofObject: object)
		equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecode9 [

	| object |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 9.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 15.

	machineSimulator temporaryRegisterValue: memory trueObject.
	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: memory nilObject.
	self assert: (memory fetchPointer: 9 ofObject: object) equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeCallImmutableTrampoline [

	| object value immutableTrampoline bytecodePC |

	cogit ceStoreCheckTrampoline: fakeTrampoline.
	immutableTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceDeoptimiseFrameTrampoline: immutableTrampoline.

	bytecodePC := 87.
	cogit bytecodePC: bytecodePC.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.	
	memory setIsImmutableOf: object to: true.
	
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self
		prepareStackForPrimitiveReceiver: object
		arguments: #(  )
		method: 0.

	"Should arrive to trampoline to put the object in the remembered set"
	self runFrom: cogInitialAddress until: immutableTrampoline.
	
	self assert: machineSimulator receiverRegisterValue equals: bytecodePC
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeCallRememberTrampoline [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self
		prepareStackForPrimitiveReceiver: object
		arguments: #(  )
		method: 0.

	"Should arrive to trampoline to put the object in the remembered set"
	self runFrom: cogInitialAddress until: storeCheckTrampoline.
	
	self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeImmutable [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory setIsImmutableOf: object to: true.

	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: 87.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: memory nilObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeRemember [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeRememberedReceiver [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	memory fromOldSpaceRememberedSet remember: object.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeRememberedReceiverBuggyIndex [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 8.
	cogit extA: 0.

	self compileBytecode: [ 
		(cogit isKindOf: StackToRegisterMappingCogit)
			ifTrue: [ cogit ssPushRegister: TempReg ]
			ifFalse: [ cogit PushR: TempReg ].
		self compileBytecodeSelector: #extStoreReceiverVariableBytecode.
		cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 9.
	memory fromOldSpaceRememberedSet remember: object.
	value := (memory integerObjectOf: 42).

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 8 ofObject: object) equals: value
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testPushReceiver [

	| object |
	self compileBytecode: [
		self compileBytecodeSelector: #pushReceiverBytecode.
		cogit genReturnTopFromMethod ].
	
	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testPushReceiverVariableBytecode [

	| object |
	self compileBytecode: [
		self compileBytecodeSelector: #pushReceiverVariableBytecode version: 0.
		cogit genReturnTopFromMethod ].
	
	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testPushReceiverVariableBytecode2 [

	| object |
	self compileBytecode: [
		self compileBytecodeSelector: #pushReceiverVariableBytecode version: 1.
		cogit genReturnTopFromMethod ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 1 ofObject: object withValue: 17.
	
	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testPushTrueObject [

	| object |
	self compileBytecode: [
		self compileBytecodeSelector: #pushConstantTrueBytecode.
		cogit genReturnTopFromMethod ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testReturnReceiver [

	 | object |
	 self compileBytecode: [
		 self compileBytecodeSelector: #returnReceiver ].

	 object := self newObjectWithSlots: 0.

	 self executePrimitiveWithReceiver: object.
	 self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testReturnTopFromMethod [

	 self compileBytecode: [
		 cogit ssPushConstant: 17.
		 self compileBytecodeSelector: #returnTopFromMethod ].

	self executePrimitiveWithReceiver: memory nilObject.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecode0 [

	self testStoreAndPopReceiverVariableBytecode: 0
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecode7 [

	self testStoreAndPopReceiverVariableBytecode: 7
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecode: index [

	| object |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	 self compileBytecode: [
		cogit ssPushRegister: TempReg. 
		self compileBytecodeSelector: #storeAndPopReceiverVariableBytecode version: index.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: index + 1.

	machineSimulator temporaryRegisterValue: memory trueObject.
	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: index ofObject: object) equals: memory trueObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeCallImmutableTrampoline [

	| object value immutableTrampoline bytecodePC valueToStore |

	cogit ceStoreCheckTrampoline: fakeTrampoline.
	immutableTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceDeoptimiseFrameTrampoline: immutableTrampoline.
	bytecodePC := 87.
	cogit bytecodePC: bytecodePC.

	 self compileBytecode: [
		cogit ssPushRegister: TempReg. 
		self compileBytecodeSelector: #storeAndPopReceiverVariableBytecode version: 0.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.	
	memory setIsImmutableOf: object to: true.

	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self
		prepareStackForPrimitiveReceiver: object
		arguments: #(  )
		method: 0.

	"Should arrive to trampoline to put the object in the remembered set"
	self runFrom: cogInitialAddress until: immutableTrampoline.

	self assert: machineSimulator receiverRegisterValue equals: bytecodePC.
	
	valueToStore := machineSimulator hasLinkRegister 
		ifTrue: [ machineSimulator smalltalkStackPointerValue ]
		ifFalse: [ machineSimulator smalltalkStackPointerValueAt: 1 ].
	self assert: valueToStore equals: value
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeCallRememberTrampoline [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	 self compileBytecode: [
		cogit ssPushRegister: TempReg. 
		self compileBytecodeSelector: #storeAndPopReceiverVariableBytecode version: 0.
		cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self
		prepareStackForPrimitiveReceiver: object
		arguments: #(  )
		method: 0.

	"Should arrive to trampoline to put the object in the remembered set"
	self runFrom: cogInitialAddress until: storeCheckTrampoline.
	
	self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeImmutable [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	 self compileBytecode: [
		cogit ssPushRegister: TempReg. 
		self compileBytecodeSelector: #storeAndPopReceiverVariableBytecode version: 0.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory setIsImmutableOf: object to: true.

	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: 87.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: memory nilObject
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeNotRemember [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	 self compileBytecode: [
		cogit ssPushRegister: TempReg. 
		self compileBytecodeSelector: #storeAndPopReceiverVariableBytecode version: 0.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeRemember [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	 self compileBytecode: [
		cogit ssPushRegister: TempReg. 
		self compileBytecodeSelector: #storeAndPopReceiverVariableBytecode version: 0.
		cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeRememberedReceiver [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	 self compileBytecode: [
		cogit ssPushRegister: TempReg. 
		self compileBytecodeSelector: #storeAndPopReceiverVariableBytecode version: 0.
		cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	memory fromOldSpaceRememberedSet remember: object.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : 'tests' }
DRProductionBytecodeTest >> testStoreAndPopRemoteTempLongBytecode [

	| object vectIndex tempIndex |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	vectIndex := 3.
	tempIndex := 1.
	
	cogit byte1: vectIndex.
	cogit byte2: tempIndex.

	 self compileBytecode: [
		self initializeSStack: 0.
		self compileBytecodeSelector: #storeAndPopRemoteTempLongBytecode.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: vectIndex + 1.

	self 
		createFramefulCallFrom: callerAddress 
		receiver: memory trueObject
		arguments: #() 
		temporaries: { object }.
	self pushAddress: memory nilObject.
	
	self executePrimitiveWithReceiver: memory trueObject. "????"

	self assert: machineSimulator receiverRegisterValue equals: memory trueObject.
	
	self assert: (memory fetchPointer: vectIndex ofObject: object) equals: memory trueObject
]
