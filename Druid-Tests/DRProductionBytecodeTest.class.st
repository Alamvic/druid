Class {
	#name : #DRProductionBytecodeTest,
	#superclass : #DRBytecodeCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeAdd [

	"We do not support the static type prediction yet"

	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 1 put: sendTrampolineAddress.
	
	self
		compileBytecode: 96
		selector: #bytecodePrimAdd
		thenDo: [ :generator | 
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 42) }.
	self
		assert:
		(memory integerValueOf: machineSimulator receiverRegisterValue)
		equals: 42 + 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeAddOverflow [

	sendTrampolineAddress := self compile: [ "In case of overflow, this routine is called and returns the receiver unchanged"
		cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 1 put: sendTrampolineAddress.

	self
		compileBytecode: 96
		selector: #bytecodePrimAdd
		thenDo: [ :generator | 
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	"Should run, overflow, jump to the trampoline, and 17"
	self
		executePrimitiveWithReceiver:
		(memory integerObjectOf: memory maxSmallInteger)
		withArguments: { (memory integerObjectOf: 1) }.
	
	self
		assert: (memory integerValueOf: machineSimulator receiverRegisterValue)
		equals: memory maxSmallInteger
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeDuplicateTop [

	self compileBytecode: 83 selector: #duplicateTopBytecode thenDo: [ :generator |
		cogit ssPushRegister: TempReg.
		
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssPopTopToReg: ReceiverResultReg.
		cogit ssPop: 1.
		cogit genUpArrowReturn ].

	machineSimulator temporaryRegisterValue: 17.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 42).
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeExtPushInteger [

	cogit extB: 1. "256"
	cogit byte1: 13.

	self
		compileBytecode: 232
		selector: #extPushIntegerBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			generator value.
			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 256 + 13).

	self assert: cogit extB equals: 0
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeExtendedPushBytecode0 [

	| method variableType variableIndex object |
	method := methodBuilder newMethod buildMethod.
	cogit methodObj: method.

	variableType := 0. "PushReceiverVariable"
	variableIndex := 0.

	cogit byte1: variableType << 6 + variableIndex.

	self
		compileBytecode: 999
		selector: #extendedPushBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executeMethodWithReceiver: object withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
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

	self
		compileBytecode: 999
		selector: #extendedPushBytecode
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

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeExtendedPushBytecode2 [

	| method variableType variableIndex |
	method := methodBuilder newMethod
		          literals: { 42 };
		          buildMethod.
	cogit methodObj: method.

	variableType := 2. "PushLiteralIndex"
	variableIndex := 0.

	cogit byte1: variableType << 6 + variableIndex.

	self
		compileBytecode: 999
		selector: #extendedPushBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #tests }
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

	self compileBytecode: 235 selector: #extSendSuperBytecode thenDo: [ :generator |
		cogit methodObj: method.

		"The receiver of the send!"
		cogit ssPushRegister: ReceiverResultReg.
		cogit ssPushRegister: Arg0Reg.

		"Execute the druid's compiled code"
		generator value.

		cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #(  )
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: cogInitialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: selectorIndex.
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeExtendedUnconditionalJump [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 176 90 "return false" 91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.
		
	cogit ceCheckForInterruptTrampoline: (self compile: [ cogit RetN: 0 ]).
		
	cogit byte1: 0.

	self
		compileBytecode: 237
		selector: #extUnconditionalJump
		thenDo: [ :generator | 
			cogit methodObj: method.
			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 16.
			generator value.
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
	
	self assert: (cogit hasAnnotatedAbstractInstructions: cogit getIsBytecodePCReference)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeExtendedUnconditionalJumpBackward [

	| method bytecodeAddress |
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 91 "return true" 176 90 "return false"];
		numberOfTemporaries: 2;
		buildMethod.
		
	cogit ceCheckForInterruptTrampoline: (self compile: [ cogit RetN: 0 ]).
		
	cogit byte1: -3.

	self
		compileBytecode: 237
		selector: #extUnconditionalJump
		thenDo: [ :generator | 
			cogit methodObj: method.
			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).
			
			cogit initializeFixupAt: 16.
			cogit bytecodePC: 16.
			cogit mergeWithFixupIfRequired: (cogit fixupAt: 16).
			cogit genReturnTrue.

			"Execute the druid's compiled code"
			bytecodeAddress := cogit Label.
			cogit bytecodePC: 17.
			generator value.
			self assert: cogit deadCode.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit genReturnFalse.
			cogit Stop.
	 	].
	
	cogInitialAddress := bytecodeAddress address.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 17).

	self assert: machineSimulator receiverRegisterValue equals: memory trueObject.
	
	self assert: (cogit hasAnnotatedAbstractInstructions: cogit getIsBytecodePCReference)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfDifferentIntegers [

	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 42) }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfEqualIntegers [

	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 17) }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjects [

	| oop |
	oop := self newObjectWithSlots: 0.
	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: oop withArguments: { oop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjectsWithBothForwarders [

	| oop forwardedOop forwardedOop2 oop2 |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	forwardedOop2 := self newObjectWithSlots: 0.
	oop2 := memory pinObject: forwardedOop2.
	self assert: (memory isForwarded: forwardedOop2).
	
	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: forwardedOop2 withArguments: { forwardedOop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjectsWithForwarderArgument [

	| oop forwardedOop oop2 |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.
	
	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: oop2 withArguments: { forwardedOop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfNonIdenticalObjects [

	| oop oop2 |
	oop := self newObjectWithSlots: 0.
	oop2 := self newObjectWithSlots: 0.
	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: oop withArguments: { oop2 }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeIdenticalOfNonIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop oop2 |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.
	
	self
		compileBytecode: 118
		selector: #bytecodePrimIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop2 }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNonIdenticalOfDifferentIntegers [

	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 42) }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfEqualIntegers [

	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 17) }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjects [

	| oop |
	oop := self newObjectWithSlots: 0.
	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: oop withArguments: { oop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjectsWithBothForwarders [

	| oop forwardedOop forwardedOop2 oop2 |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	forwardedOop2 := self newObjectWithSlots: 0.
	oop2 := memory pinObject: forwardedOop2.
	self assert: (memory isForwarded: forwardedOop2).
	
	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: forwardedOop2 withArguments: { forwardedOop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjectsWithForwarderArgument [

	| oop forwardedOop oop2 |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.
	
	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: oop2 withArguments: { forwardedOop }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop }.
	self deny:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfNonIdenticalObjects [

	| oop oop2 |
	oop := self newObjectWithSlots: 0.
	oop2 := self newObjectWithSlots: 0.
	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: oop withArguments: { oop2 }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #'tests - identical' }
DRProductionBytecodeTest >> testBytecodeNotIdenticalOfNonIdenticalObjectsWithForwarderReceiver [

	| oop forwardedOop oop2 |
	"We need to setup the stack to forward objects"
	self createBaseFrame.
	
	forwardedOop := self newObjectWithSlots: 0.
	oop := memory pinObject: forwardedOop.
	self assert: (memory isForwarded: forwardedOop).
	
	oop2 := self newObjectWithSlots: 0.
	
	self
		compileBytecode: 120
		selector: #bytecodePrimNotIdenticalSistaV1
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: forwardedOop withArguments: { oop2 }.
	self assert:
		(memory booleanValueOf: machineSimulator receiverRegisterValue)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePrimSendAdd [

	"Setup the send trampoline"
	| method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 1 "arg" put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	self
		compileBytecode: 96
		selector: #bytecodePrimAdd
		thenDo: [ :generator | 
			cogit methodObj: method.
			"The receiver of the send!"
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushConstant: (memory integerObjectOf: 42).

			"Execute the druid's compiled code"
			generator value.

			cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: cogInitialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: (-1 twoComplementOfBitSize: 64) "+ selector index".
	"Third argument: the receiver, then the argument is on the stack"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17).
	self assert: machineSimulator arg0RegisterValue equals: (memory integerObjectOf: 42)
	



]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushConstantOneBytecode [

	| method |
	method := methodBuilder
		          newMethod
		          buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 81
		selector: #pushConstantOneBytecode
		thenDo: [ :generator | 
			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 1)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushConstantZeroBytecode [

	| method |
	method := methodBuilder
		          newMethod
		          buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 80
		selector: #pushConstantZeroBytecode
		thenDo: [ :generator | 
			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 0)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant [

	| literalIndex method |
	method := methodBuilder newMethod
		          literals: { (memory integerObjectOf: 42) };
		          buildMethod.

	cogit methodObj: method.

	literalIndex := 0. "0 based..."
	self
		compileBytecode: 32 + literalIndex
		selector: #pushLiteralConstantBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			(cogit isKindOf: StackToRegisterMappingCogit)
				ifTrue: [ cogit ssPopTopToReg: ReceiverResultReg ]
				ifFalse: [ cogit PopR: ReceiverResultReg ].
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 42).

	self assert:
		(cogit hasAnnotatedAbstractInstructions: cogit getIsObjectReference)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant2 [

	| literalIndex method |

	method := methodBuilder
		          newMethod
		          literals: { (memory integerObjectOf: 42). (memory integerObjectOf: 43) };
		          buildMethod.
		
	cogit methodObj: method.

	literalIndex := 1 "0 based...".
	self compileBytecode: 32 + literalIndex selector: #pushLiteralConstantBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssPopTopToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].


	self executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #().
		
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 43).
	
	self assert:
		(cogit hasAnnotatedAbstractInstructions: cogit getIsObjectReference)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable0 [

	self testBytecodePushLiteralVariable: 0
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable1 [

	self testBytecodePushLiteralVariable: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable15 [

	self testBytecodePushLiteralVariable: 15
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable: tempIndex [

	| method |
	method := methodBuilder newMethod
		          literals:
			          (((1 to: 16) collect: [ :e | nil -> (e + 16rFF - 1) ])
				           collect: [ :e | e forMemory: memory inMethod: nil ]);
		          buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 16 + tempIndex
		selector: #pushLiteralVariable16CasesBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssPopTopToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: tempIndex + 16rFF).

	self assert:
		(cogit hasAnnotatedAbstractInstructions: cogit getIsObjectReference)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTop2AllocatedRegister [

	self
		testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: 2
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTop3AllocatedRegister [

	self
		testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: 3
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister [

	
	self testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariableDoesOverrideTopAllocatedRegister: n [

	| method |
	method := methodBuilder newMethod
		          literals:
			          (((1 to: 16) collect: [ :e | nil -> (e + 16rFF - 1) ])
				           collect: [ :e | e forMemory: memory inMethod: nil ]);
		          buildMethod.

	cogit methodObj: method.

	"Push temp literal variable 0"
	self
		compileBytecode: 16
		selector: #pushLiteralVariable16CasesBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 0.
			cogit methodOrBlockNumTemps: 0.
			cogit initSimStackForFramefulMethod: 2.

			1 to: n do: [ :i | | r |
				r := cogit allocateRegNotConflictingWith: 0.
				cogit MoveCq: i R: r.
				cogit ssPushRegister: r.
			].

			generator value.
			
			"Flush the stack"
			cogit ssFlushTo: cogit simStackPointer.

			"Pop the top 4 elements, the fourth should be the first pushed constant"
			1 to: n + 1 do: [ :i |
				cogit ssPopTopToReg: ReceiverResultReg ].
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushReceiverDoesNotOverridePreviousPush [

	| method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 0;
		          buildMethod.

	cogit methodObj: method.

	"Push temporary 0 -- aka first argument if there are arguments"
	self doCompileBytecode: 64 selector: #pushTemporaryVariableBytecode.

	self compileBytecode: 76 selector: #pushReceiverBytecode thenDo: [ :generator |
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 1.
			cogit initSimStackForFramefulMethod: 2.

			"Push argument 0"
			cogit gen_pushTemporaryVariableBytecode.

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

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
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

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeReturnFalse [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 90
		selector: #returnFalse
		thenDo: [ :generator | "Execute the druid's compiled code"
		generator value ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory falseObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeReturnNil [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 91
		selector: #returnNil
		thenDo: [ :generator | "Execute the druid's compiled code"
		generator value ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory nilObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeReturnTrue [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 89
		selector: #returnTrue
		thenDo: [ :generator | "Execute the druid's compiled code"
		generator value ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeSendLiteral0ToSelf [

	"128 - sendLiteralSelector0ArgsBytecode selector=literal0, numargs=0.
	Test we do a send and we end up in a trampoline"
	
	"Setup the send trampoline"
	| method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 0 put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	self
		compileBytecode: 128 + 0 "selector index"
		selector: #sendLiteralSelector0ArgsBytecode
		thenDo: [ :generator | 
			cogit methodObj: method.
			"The receiver of the send!"
			cogit ssPushRegister: ReceiverResultReg.

			"Execute the druid's compiled code"
			generator value.

			cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: cogInitialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: 0 "selector index".
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeSendLiteral1ToSelf [

	"129 - sendLiteralSelector0ArgsBytecode selector=literal0, numargs=0.
	Test we do a send and we end up in a trampoline"
	
	"Setup the send trampoline"
	| method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 0 put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			memory nilObject.
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	self
		compileBytecode: 128 + 1 "selector index"
		selector: #sendLiteralSelector0ArgsBytecode
		thenDo: [ :generator | 
			cogit methodObj: method.
			"The receiver of the send!"
			cogit ssPushRegister: ReceiverResultReg.

			"Execute the druid's compiled code"
			generator value.

			cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: cogInitialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: 1 "selector index".
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : #tests }
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

	self
		compileBytecode: 192
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.
			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	self executePrimitiveWithReceiver: memory falseObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #tests }
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

	self
		compileBytecode: 192
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.
			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	self
		prepareStackForSendReceiver: memory nilObject
		arguments: #()
		method: method.
	self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress.

	self assert: machineSimulator receiverRegisterValue equals: memory nilObject
]

{ #category : #tests }
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

	self
		compileBytecode: 192
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	"self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
	self runFrom: initialAddress until: mustBeBooleanTrampolineAddress."
	self executePrimitiveWithReceiver: memory trueObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #tests }
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

	self
		compileBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	"self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
	self runFrom: initialAddress until: mustBeBooleanTrampolineAddress."
	self executePrimitiveWithReceiver: memory falseObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #tests }
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

	self
		compileBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	self
		prepareStackForSendReceiver: memory falseObject
		arguments: #()
		method: method.

	self should: [ self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress ] raise: Exception 
]

{ #category : #tests }
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

	self
		compileBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	self
		prepareStackForSendReceiver: memory nilObject
		arguments: #()
		method: method.
	self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress.

	self assert: machineSimulator receiverRegisterValue equals: memory nilObject
]

{ #category : #tests }
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

	self
		compileBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	"self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
	self runFrom: initialAddress until: mustBeBooleanTrampolineAddress."
	self executePrimitiveWithReceiver: memory trueObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #tests }
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

	self
		compileBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
		
	self should: [ self runFrom: cogInitialAddress until: mustBeBooleanTrampolineAddress ] raise: Exception 
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeShortUnconditionalJump [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 176 90 "return false" 91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 176
		selector: #shortUnconditionalJump
		thenDo: [ :generator | 
			cogit methodObj: method.
			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 16.
			generator value.
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
	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtBBytecode [

	self
		compileBytecode: 225
		selector: #extBBytecode
		thenDo: [ :generator | cogit genUpArrowReturn ].

	cogit byte1: 1.
	cogit extB: 7.

	cogit gen_extBBytecode.

	self assert: cogit extB equals: 7 << 8 + 1.
	self assert: cogit numExtB equals: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtBBytecode2 [

	self
		compileBytecode: 225
		selector: #extBBytecode
		thenDo: [ :generator | cogit genUpArrowReturn ].

	cogit byte1: 256.
	cogit extB: 7.

	cogit gen_extBBytecode.

	self assert: cogit extB equals: 0.
	self assert: cogit numExtB equals: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtBBytecode3 [

	self
		compileBytecode: 225
		selector: #extBBytecode
		thenDo: [ :generator | cogit genUpArrowReturn ].

	cogit byte1: 128.
	cogit extB: 7.

	cogit gen_extBBytecode.

	self assert: cogit extB equals: -128.
	self assert: cogit numExtB equals: 1
]

{ #category : #tests }
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

	self
		compileBytecode: 244
		selector: #extStoreLiteralVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: memory trueObject.

	self assert: machineSimulator receiverRegisterValue equals: memory trueObject.
	
	self assert: (memory fetchPointer: 1 ofObject: literal) equals: memory trueObject
]

{ #category : #tests }
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

	self
		compileBytecode: 244
		selector: #extStoreLiteralVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: ReceiverResultReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
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

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecode [

	| object |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			(cogit isKindOf: StackToRegisterMappingCogit)
				ifTrue: [ cogit ssPushRegister: TempReg ]
				ifFalse: [ cogit PushR: TempReg ].

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.

	machineSimulator temporaryRegisterValue: memory trueObject.
	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self
		assert: (memory fetchPointer: 0 ofObject: object)
		equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecode9 [

	| object |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 9.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 15.

	machineSimulator temporaryRegisterValue: memory trueObject.
	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: memory nilObject.
	self assert: (memory fetchPointer: 9 ofObject: object) equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeCallImmutableTrampoline [

	| object value immutableTrampoline bytecodePC |

	cogit ceStoreCheckTrampoline: fakeTrampoline.
	immutableTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceDeoptimiseFrameTrampoline: immutableTrampoline.

	bytecodePC := 87.
	cogit bytecodePC: bytecodePC.
	cogit byte1: 0.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
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

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeCallRememberTrampoline [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
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

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeImmutable [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory setIsImmutableOf: object to: true.

	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: 87.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: memory nilObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeRemember [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeRememberedReceiver [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 0.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	memory fromOldSpaceRememberedSet remember: object.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : #tests }
DRProductionBytecodeTest >> testExtStoreReceiverVariableBytecodeRememberedReceiverBuggyIndex [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.

	cogit bytecodePC: 87.
	cogit byte1: 8.
	cogit extA: 0.

	self
		compileBytecode: 243
		selector: #extStoreReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 9.
	memory fromOldSpaceRememberedSet remember: object.
	value := (memory integerObjectOf: 42).

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 8 ofObject: object) equals: value
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiver [

	| object |
	self compileBytecode: 76 selector: #pushReceiverBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.

		"Then return without druid's compiled code"
		cogit ssPopTopToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiverVariableBytecode [

	| object |
	self compileBytecode: 0 selector: #pushReceiverVariableBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssPopTopToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiverVariableBytecode2 [

	| object |
	self compileBytecode: 1 selector: #pushReceiverVariableBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssPopTopToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 1 ofObject: object withValue: 17.
	
	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushTrueObject [

	| object |
	self compileBytecode: 77 selector: #pushConstantTrueBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssPopTopToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testReturnReceiver [

	| object |
	self compileBytecode: 88 selector: #returnReceiver thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value ].

	object := self newObjectWithSlots: 0.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : #tests }
DRProductionBytecodeTest >> testReturnTopFromMethod [

	| object |
	self compileBytecode: 92 selector: #returnTopFromMethod thenDo: [ :generator |
		cogit ssPushConstant: 17.

		"Execute the druid's compiled code"
		generator value ].

	object := self newObjectWithSlots: 0.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecode0 [

	self testStoreAndPopReceiverVariableBytecode: 0
]

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecode7 [

	self testStoreAndPopReceiverVariableBytecode: 7
]

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecode: index [

	| object |
	cogit ceStoreCheckTrampoline: fakeTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	self
		compileBytecode: 200 + index
		selector: #storeAndPopReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newObjectWithSlots: index + 1.

	machineSimulator temporaryRegisterValue: memory trueObject.
	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: index ofObject: object) equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeCallImmutableTrampoline [

	| object value immutableTrampoline bytecodePC |

	cogit ceStoreCheckTrampoline: fakeTrampoline.
	immutableTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceDeoptimiseFrameTrampoline: immutableTrampoline.
	bytecodePC := 87.
	cogit bytecodePC: bytecodePC.

	self
		compileBytecode: 200
		selector: #storeAndPopReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
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

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeCallRememberTrampoline [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	self
		compileBytecode: 200
		selector: #storeAndPopReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
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

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeImmutable [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	self
		compileBytecode: 200
		selector: #storeAndPopReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory setIsImmutableOf: object to: true.

	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: 87.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: memory nilObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeNotRemember [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	self
		compileBytecode: 200
		selector: #storeAndPopReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeRemember [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	self
		compileBytecode: 200
		selector: #storeAndPopReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]

{ #category : #tests }
DRProductionBytecodeTest >> testStoreAndPopReceiverVariableBytecodeRememberedReceiver [

	| object value storeCheckTrampoline |

	storeCheckTrampoline := self compile: [ cogit RetN: 0 ].
	cogit ceStoreCheckTrampoline: storeCheckTrampoline.
	cogit ceDeoptimiseFrameTrampoline: fakeTrampoline.
	cogit bytecodePC: 87.

	self
		compileBytecode: 200
		selector: #storeAndPopReceiverVariableBytecode
		thenDo: [ :generator |
			cogit ssPushRegister: TempReg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit genUpArrowReturn ].

	object := self newOldSpaceObjectWithSlots: 1.
	memory fromOldSpaceRememberedSet remember: object.
	value := self newObjectWithSlots: 0.

	machineSimulator temporaryRegisterValue: value.

	self executePrimitiveWithReceiver: object.

	self assert: machineSimulator receiverRegisterValue equals: object.
	self assert: (memory fetchPointer: 0 ofObject: object) equals: value
]
