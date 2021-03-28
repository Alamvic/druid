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

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jump9 jump10 jump11 jump12 jump13 jump14 jump15 jump16 jump17 jump18 jump19 jump20 |
	currentBlock := self Label.
	self MoveR: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: 1 R: SendNumArgsReg.
	self TstCq: ClassReg R: SendNumArgsReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveR: -63 R: Extra1Reg.
	self MoveR: ClassReg R: Extra2Reg.
	self LogicalShiftLeftCq: Extra1Reg R: Extra2Reg.
	self MoveR: 1 R: TempReg.
	self CmpR: Extra2Reg R: TempReg.
	jump1 := self JumpZero: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	jump2 := self Jump: 0.
	self MoveR: 0 R: Extra0Reg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	self MoveR: 3 R: Extra1Reg.
	self MoveCq: Extra1Reg R: SendNumArgsReg.
	self NegateR: SendNumArgsReg.
	self MoveR: ClassReg R: TempReg.
	self LogicalShiftLeftCq: SendNumArgsReg R: TempReg.
	self MoveR: 16r1FFFFFFFFFFFFFFF R: Extra2Reg.
	self AndR: TempReg R: Extra0Reg.
	self MoveR: 16r1FFFFFFFFFFFFFFF R: Extra1Reg.
	self SubR: Extra0Reg R: SendNumArgsReg.
	self MoveR: 1 R: TempReg.
	self SubR: SendNumArgsReg R: Extra2Reg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: 3 R: Extra0Reg.
	self MoveCq: Extra0Reg R: Extra1Reg.
	self NegateR: Extra1Reg.
	self MoveR: ClassReg R: TempReg.
	self LogicalShiftLeftCq: Extra1Reg R: TempReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: 0 R: SendNumArgsReg.
	self MoveR: Arg0Reg R: Extra0Reg.
	self MoveR: 1 R: Extra1Reg.
	self TstCq: Extra0Reg R: Extra1Reg.
	jump6 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	self MoveR: -63 R: Extra1Reg.
	self MoveR: Extra0Reg R: ClassReg.
	self LogicalShiftLeftCq: Extra1Reg R: ClassReg.
	self MoveR: 1 R: TempReg.
	self CmpR: ClassReg R: TempReg.
	jump8 := self JumpZero: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump9 := self Jump: 0.
	self MoveR: 0 R: SendNumArgsReg.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	self MoveR: 3 R: Extra1Reg.
	self MoveCq: Extra1Reg R: Extra2Reg.
	self NegateR: Extra2Reg.
	self MoveR: Extra0Reg R: TempReg.
	self LogicalShiftLeftCq: Extra2Reg R: TempReg.
	self MoveR: 16r1FFFFFFFFFFFFFFF R: ClassReg.
	self AndR: TempReg R: SendNumArgsReg.
	self MoveR: 16r1FFFFFFFFFFFFFFF R: Extra1Reg.
	self SubR: SendNumArgsReg R: Extra2Reg.
	self MoveR: 1 R: TempReg.
	self SubR: Extra2Reg R: ClassReg.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveR: 3 R: SendNumArgsReg.
	self MoveCq: SendNumArgsReg R: Extra1Reg.
	self NegateR: Extra1Reg.
	self MoveR: Extra0Reg R: TempReg.
	self LogicalShiftLeftCq: Extra1Reg R: TempReg.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	self AddR: Phixx R: Extra2Reg.
	jump13 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	jump12 jmpTarget: currentBlock.
	jump14 := self Jump: 0.
	currentBlock := self Label.
	self MoveR: 60 R: Extra0Reg.
	self LogicalShiftRightCq: Extra2Reg R: Extra1Reg.
	self MoveR: 1 R: ClassReg.
	self AddR: Extra1Reg R: TempReg.
	self MoveR: 15 R: SendNumArgsReg.
	self AndR: TempReg R: Extra0Reg.
	self MoveR: 1 R: Extra2Reg.
	self CmpR: Extra0Reg R: Extra2Reg.
	jump15 := self JumpLessOrEqual: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump16 := self Jump: 0.
	currentBlock := self Label.
	jump15 jmpTarget: currentBlock.
	self MoveR: false R: ClassReg.
	jump17 := self Jump: 0.
	jump18 := self Jump: 0.
	currentBlock := self Label.
	jump16 jmpTarget: currentBlock.
	jump18 jmpTarget: currentBlock.
	jump19 := self Jump: 0.
	jump20 := self Jump: 0
]

{ #category : #garbage }
DruidCogit >> minCallAddress: anInteger [ 
	
	"Nothing"
]

{ #category : #accessing }
DruidCogit >> objectRepresentationClass [

	^ self
]
