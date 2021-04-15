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

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jump9 jump10 jump11 jump12 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self MoveCq: 1 R: Extra1Reg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra1Reg.
	self TstCq: 1 R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self TstCq: 1 R: ClassReg.
	jump2 := self JumpZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: 1 R: TempReg.
	self TstCq: 1 R: Extra0Reg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self TstCq: 1 R: Extra0Reg.
	jump6 := self JumpZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: Extra1Reg.
	self MoveR: ClassReg R: Extra2Reg.
	self SubR: Extra1Reg R: Extra2Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: ClassReg R: Extra2Reg.
	self SubR: Extra1Reg R: Extra2Reg.
	jump9 := self Jump: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self MoveR: ClassReg R: Extra2Reg.
	self SubR: Extra1Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self AddR: Extra0Reg R: TempReg.
	jump10 := self JumpNoOverflow: 0.
	currentBlock := self Label.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: TempReg.
	self AddR: Extra0Reg R: TempReg.
	self MoveCq: 2 R: ClassReg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	jump11 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump12 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidCogit >> genPrimitiveAdd2 [

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jump9 jump10 jump11 jump12 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self MoveCq: 1 R: Extra1Reg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra1Reg.
	self TstCq: 1 R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self TstCq: 1 R: ClassReg.
	jump2 := self JumpZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: 1 R: TempReg.
	self TstCq: 1 R: Extra0Reg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self TstCq: 1 R: Extra0Reg.
	jump6 := self JumpZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: Extra1Reg.
	self SubR: ClassReg R: Extra2Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self SubR: ClassReg R: Extra2Reg.
	jump9 := self Jump: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self SubR: ClassReg R: Extra2Reg.
	self AddR: Extra2Reg R: TempReg.
	jump10 := self JumpOverflow: 0.
	currentBlock := self Label.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	self AddR: Extra2Reg R: TempReg.
	self MoveCq: 2 R: ClassReg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	jump11 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump12 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize
]

{ #category : #garbage }
DruidCogit >> minCallAddress: anInteger [ 
	
	"Nothing"
]

{ #category : #accessing }
DruidCogit >> objectRepresentationClass [

	^ self
]
