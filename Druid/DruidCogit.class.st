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
DruidCogit >> genPrimitiveAdd2 [

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 |
	self MoveCq: 0 R: R1.
	self MoveR: Arg0Reg R: R2.
	self MoveCq: 1 R: R3.
	self MoveR: ReceiverResultReg R: R4.
	self MoveCq: 1 R: R7.
	self TstCq: 1 R: R2.
	jump0 := self JumpZero: 0.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: false R: R9.
	self MoveR: R19 R: R25.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: R12.
	self TstCq: 1 R: R4.
	jump3 := self JumpZero: 0.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: false R: R14.
	self MoveR: R19 R: R25.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveCq: 1 R: R16.
	self MoveR: R2 R: R17.
	self SubCq: 1 R: R17.
	self MoveR: R17 R: R19.
	self AddR: R4 R: R19.
	self MoveR: R19 R: R23.
	jump6 := self JumpNoOverflow: 0.
	self MoveCq: false R: R21.
	self MoveNull: NULL R: R22.
	self MoveR: R22 R: R23.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveCq: 2 R: R24.
	self MoveR: R19 R: R25.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	self MoveR: R19 R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
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
