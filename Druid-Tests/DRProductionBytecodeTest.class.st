Class {
	#name : #DRProductionBytecodeTest,
	#superclass : #DRBytecodeCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeAdd [

	self compileBytecode: 96 selector: #bytecodePrimAdd thenDo: [ :generator |
		cogit ssPushRegister: ReceiverResultReg.
		cogit ssPushRegister: TempReg.
		
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit ssPop: 1.
		cogit genUpArrowReturn ].

	machineSimulator receiverRegisterValue: 0.
	machineSimulator receiverRegisterValue: 1.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 17).
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
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
