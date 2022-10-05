Class {
	#name : #DRProductionBytecodeTest,
	#superclass : #DRBytecodeCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiver [

	| object |
	self compileBytecode: 76 selector: #pushReceiverBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.

		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit ssPop: 1.
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
		cogit ssPop: 1.
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
		cogit ssPop: 1.
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
		cogit ssPop: 1.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]
