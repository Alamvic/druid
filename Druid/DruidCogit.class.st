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

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jump9 jump10 jump11 jump12 jump13 jump14 jump15 jump16 jump17 jump18 |
	self MoveCq: 0 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveCq: 1 R: Extra0Reg.
	self TstCq: 1 R: ClassReg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MoveR: ClassReg R: Extra2Reg.
	jump0 := self JumpZero: 0.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self CmpCq: 0 R: TempReg.
	self MoveR: SendNumArgsReg R: ClassReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self TstCq: 1 R: Extra1Reg.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	jump9 := self Jump: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	jump10 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	jump11 := self Jump: 0.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: Extra2Reg R: Extra0Reg.
	jump13 := self JumpZero: 0.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	jump12 jmpTarget: currentBlock.
	jump14 := self Jump: 0.
	jump15 := self Jump: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump14 jmpTarget: currentBlock.
	jump16 := self Jump: 0.
	currentBlock := self Label.
	jump15 jmpTarget: currentBlock.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: Extra0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	jump17 := self Jump: 0.
	currentBlock := self Label.
	jump16 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: R51.
	self MoveR: ReceiverResultReg R: R52.
	jump18 := self Jump: 0.
	currentBlock := self Label.
	jump17 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self AddR: ClassReg R: SendNumArgsReg.
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
