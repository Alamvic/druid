Class {
	#name : #DRProductionBytecodeTest,
	#superclass : #DRBytecodeCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeAdd [

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
			cogit ssTop popToReg: ReceiverResultReg.
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
DRProductionBytecodeTest >> testBytecodeDuplicateTop [

	self compileBytecode: 83 selector: #duplicateTopBytecode thenDo: [ :generator |
		cogit ssPushRegister: TempReg.
		
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop moveToReg: ReceiverResultReg.
		cogit ssPop: 2.
		cogit genUpArrowReturn ].

	machineSimulator temporaryRegisterValue: 17.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 42).
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant [

	| literalIndex method |
	literalIndex := 0 "0 based...".
	self compileBytecode: 32 + literalIndex selector: #pushLiteralConstantBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		buildMethod.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #()
		method: method.
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 42)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant2 [

	| literalIndex method |
	literalIndex := 1 "0 based...".
	self compileBytecode: 32 + literalIndex selector: #pushLiteralConstantBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42). (memory integerObjectOf: 43) };
		buildMethod.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #()
		method: method.
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 43)
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
		prepareStackForSendReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: initialAddress until: sendTrampolineAddress.
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
		prepareStackForSendReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: initialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: 1 "selector index".
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiver [

	| object |
	self compileBytecode: 76 selector: #pushReceiverBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.

		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
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
		cogit ssTop popToReg: ReceiverResultReg.
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
		cogit ssTop popToReg: ReceiverResultReg.
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
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]
