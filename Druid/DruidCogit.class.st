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

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jump9 jump10 jump11 jump12 jump13 jump14 jump15 jump16 jump17 jump18 jump19 jump20 jump21 jump22 jump23 jump24 jump25 jump26 jump27 jump28 jump29 jump30 jump31 jump32 jump33 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self TstR: ClassReg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: -63 R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra0Reg.
	self LogicalShiftLeftR: SendNumArgsReg R: Extra0Reg.
	self MoveCq: 1 R: Extra1Reg.
	self CmpR: Extra0Reg R: Extra1Reg.
	jump3 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	self MoveCq: 3 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: 3 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra0Reg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: TempReg.
	self NegateR: TempReg.
	self MoveR: ClassReg R: Extra2Reg.
	self LogicalShiftLeftR: TempReg R: Extra2Reg.
	self MoveCq: 16r1FFFFFFFFFFFFFFF R: SendNumArgsReg.
	self AndR: Extra2Reg R: Extra0Reg.
	self MoveCq: 16r1FFFFFFFFFFFFFFF R: TempReg.
	self SubR: Extra0Reg R: SendNumArgsReg.
	self MoveCq: 1 R: Extra2Reg.
	self SubR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: Extra1Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra2Reg.
	self NegateR: Extra2Reg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self LogicalShiftLeftR: Extra2Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: Extra0Reg.
	jump9 := self Jump: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra2Reg.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: ClassReg.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self MoveCq: 0 R: TempReg.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self MoveCq: 1 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra0Reg.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump12 jmpTarget: currentBlock.
	self TstR: SendNumArgsReg R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	jump13 := self Jump: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	jump14 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: -63 R: Extra0Reg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self LogicalShiftLeftR: Extra0Reg R: Extra2Reg.
	self MoveCq: 1 R: Extra1Reg.
	self CmpR: Extra2Reg R: Extra1Reg.
	jump15 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump14 jmpTarget: currentBlock.
	jump16 := self Jump: 0.
	currentBlock := self Label.
	self MoveCq: 3 R: Extra1Reg.
	self MoveR: Extra1Reg R: TempReg.
	jump17 := self Jump: 0.
	currentBlock := self Label.
	jump15 jmpTarget: currentBlock.
	self MoveCq: 3 R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	jump18 := self Jump: 0.
	currentBlock := self Label.
	jump17 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra0Reg.
	self NegateR: Extra0Reg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self LogicalShiftLeftR: Extra0Reg R: Extra1Reg.
	self MoveCq: 16r1FFFFFFFFFFFFFFF R: TempReg.
	self AndR: Extra1Reg R: Extra0Reg.
	self MoveCq: 16r1FFFFFFFFFFFFFFF R: Extra1Reg.
	self SubR: Extra0Reg R: TempReg.
	self MoveCq: 1 R: Extra0Reg.
	self SubR: TempReg R: Extra1Reg.
	self MoveR: Extra1Reg R: TempReg.
	jump19 := self Jump: 0.
	currentBlock := self Label.
	jump18 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra0Reg.
	self NegateR: Extra0Reg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self LogicalShiftLeftR: Extra0Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	jump20 := self Jump: 0.
	currentBlock := self Label.
	jump19 jmpTarget: currentBlock.
	jump20 jmpTarget: currentBlock.
	self MoveR: TempReg R: SendNumArgsReg.
	jump21 := self Jump: 0.
	currentBlock := self Label.
	jump21 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	jump22 := self Jump: 0.
	currentBlock := self Label.
	jump22 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra2Reg.
	jump23 := self Jump: 0.
	currentBlock := self Label.
	jump23 jmpTarget: currentBlock.
	self AddR: ClassReg R: Extra1Reg.
	jump24 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 60 R: Extra0Reg.
	self LogicalShiftRightR: Extra1Reg R: ClassReg.
	self MoveCq: 1 R: Extra2Reg.
	self AddR: ClassReg R: TempReg.
	self MoveCq: 15 R: SendNumArgsReg.
	self AndR: TempReg R: Extra0Reg.
	self MoveCq: 1 R: Extra2Reg.
	self CmpR: Extra0Reg R: Extra2Reg.
	self CmpR: Extra0Reg R: Extra2Reg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump25 := self Jump: 0.
	currentBlock := self Label.
	jump25 jmpTarget: currentBlock.
	jump26 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 2 R: Extra2Reg.
	self MoveCq: 3 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump27 := self Jump: 0.
	currentBlock := self Label.
	jump26 jmpTarget: currentBlock.
	self MoveCq: false R: TempReg.
	jump28 := self Jump: 0.
	currentBlock := self Label.
	jump27 jmpTarget: currentBlock.
	self LogicalShiftLeftR: Extra0Reg R: Extra2Reg.
	self MoveCq: 1 R: Extra1Reg.
	self AddR: Extra2Reg R: TempReg.
	self MoveR: TempReg R: ClassReg.
	jump29 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	jump16 jmpTarget: currentBlock.
	jump28 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump29 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump30 := self Jump: 0.
	currentBlock := self Label.
	jump30 jmpTarget: currentBlock.
	jump31 := self Jump: 0.
	currentBlock := self Label.
	jump24 jmpTarget: currentBlock.
	jump31 jmpTarget: currentBlock.
	jump32 := self Jump: 0.
	currentBlock := self Label.
	jump32 jmpTarget: currentBlock.
	jump33 := self Jump: 0.
	currentBlock := self Label.
	jump33 jmpTarget: currentBlock.
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
