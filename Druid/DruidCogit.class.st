Class {
	#name : #DruidCogit,
	#superclass : #Cogit,
	#category : #'Druid-Tests'
}

{ #category : #configuration }
DruidCogit class >> initializeBytecodeTableForSistaV1 [
	
	"Do nothing for now"
]

{ #category : #configuration }
DruidCogit class >> initializePrimitiveTable [
	
	"Do nothing for now"
]

{ #category : #'simulation only' }
DruidCogit >> bytecodeFixupClass [
	<doNotGenerate>
	^CogBytecodeFixup
]

{ #category : #garbage }
DruidCogit >> entryOffset: anInteger [ 
	
	"Nothing"
]

{ #category : #garbage }
DruidCogit >> forCogit: aDruidCogit methodZone: aCogMethodZone [ 
	
	"nothing?"
]

{ #category : #generated }
DruidCogit >> genPrimitiveAdd [

	| currentBlock jump0 jump1 jump2 |
	self TstCq: 1 R: Arg0Reg.
	jump0 := self JumpZero: 0.
	self TstCq: 1 R: ReceiverResultReg.
	jump1 := self JumpZero: 0.
	self MoveR: Arg0Reg R: Extra1Reg.
	self SubCq: 1 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self AddR: ReceiverResultReg R: Extra2Reg.
	jump2 := self JumpOverflow: 0.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidCogit >> genPrimitiveSubtract [

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 |
	self TstCq: 1 R: Arg0Reg.
	jump0 := self JumpZero: 0.
	self TstCq: 1 R: ReceiverResultReg.
	jump1 := self JumpZero: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: Arg0Reg.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	self MoveR: Arg0Reg R: Extra1Reg.
	self SubR: ReceiverResultReg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self LogicalShiftRightCq: 60 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self AddCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra4Reg.
	self AndCq: 15 R: Extra4Reg.
	self CmpCq: 1 R: Extra4Reg.
	jump3 := self JumpGreater: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: Arg0Reg.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	jump4 := self Jump: 0.
	self LogicalShiftLeftR: Extra0Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self AddCq: 1 R: Extra2Reg.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: Arg0Reg.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #garbage }
DruidCogit >> minCallAddress: anInteger [ 
	
	"Nothing"
]

{ #category : #accessing }
DruidCogit >> objectRepresentationClass [

	^ self
]
