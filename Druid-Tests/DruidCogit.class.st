Class {
	#name : 'DruidCogit',
	#superclass : 'Cogit',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'configuration' }
DruidCogit class >> initializeBytecodeTableForSistaV1 [
	
	"Do nothing for now"
]

{ #category : 'configuration' }
DruidCogit class >> initializePrimitiveTable [
	
	"Do nothing for now"
]

{ #category : 'simulation only' }
DruidCogit >> bytecodeFixupClass [
	<doNotGenerate>
	^CogBytecodeFixup
]

{ #category : 'garbage' }
DruidCogit >> entryOffset: anInteger [ 
	
	"Nothing"
]

{ #category : 'garbage' }
DruidCogit >> forCogit: aDruidCogit methodZone: aCogMethodZone [ 
	
	^ objectMemory 
]

{ #category : 'generated' }
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

{ #category : 'generated' }
DruidCogit >> genPrimitiveEqual [

	| currentBlock jump0 jump1 jump2 jump3 jump4 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AndR: Arg0Reg R: ClassReg.
	self TstCq: 1 R: ClassReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra4Reg.
	jump0 := self JumpZero: 0.
	self CmpR: Arg0Reg R: TempReg.
	jump1 := self JumpZero: 0.
	jump2 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : 'generated' }
DruidCogit >> genPrimitiveGreaterOrEqual [

	| currentBlock jump0 jump1 jump2 jump3 jump4 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AndR: Arg0Reg R: ClassReg.
	self TstCq: 1 R: ClassReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra4Reg.
	jump0 := self JumpZero: 0.
	self CmpR: Arg0Reg R: TempReg.
	jump1 := self JumpGreaterOrEqual: 0.
	jump2 := self JumpLess: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: Extra0Reg.
	self MoveR: Extra0Reg R: TempReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : 'generated' }
DruidCogit >> genPrimitiveGreaterThan [

	| currentBlock jump0 jump1 jump2 jump3 jump4 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AndR: Arg0Reg R: ClassReg.
	self TstCq: 1 R: ClassReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra4Reg.
	jump0 := self JumpZero: 0.
	self CmpR: Arg0Reg R: TempReg.
	jump1 := self JumpGreater: 0.
	jump2 := self JumpLessOrEqual: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : 'generated' }
DruidCogit >> genPrimitiveLessOrEqual [

	| currentBlock jump0 jump1 jump2 jump3 jump4 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AndR: Arg0Reg R: ClassReg.
	self TstCq: 1 R: ClassReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra4Reg.
	jump0 := self JumpZero: 0.
	self CmpR: Arg0Reg R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	jump2 := self JumpGreater: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: Extra0Reg.
	self MoveR: Extra0Reg R: TempReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : 'generated' }
DruidCogit >> genPrimitiveLessThan [

	| currentBlock jump0 jump1 jump2 jump3 jump4 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AndR: Arg0Reg R: ClassReg.
	self TstCq: 1 R: ClassReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra4Reg.
	jump0 := self JumpZero: 0.
	self CmpR: Arg0Reg R: TempReg.
	jump1 := self JumpLess: 0.
	jump2 := self JumpGreaterOrEqual: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : 'generated' }
DruidCogit >> genPrimitiveSubtract [

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self TstCq: 1 R: TempReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra4Reg.
	jump0 := self JumpZero: 0.
	self TstCq: 1 R: ClassReg.
	self MoveR: Arg0Reg R: Extra2Reg.
	self MoveR: ReceiverResultReg R: Extra4Reg.
	jump1 := self JumpZero: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	self MoveR: TempReg R: SendNumArgsReg.
	self SubR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self AddCq: 1 R: Extra0Reg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	jump3 := self JumpOverflow: 0.
	jump4 := self JumpNoOverflow: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: Arg0Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : 'garbage' }
DruidCogit >> minCallAddress: anInteger [ 
	
	"Nothing"
]

{ #category : 'accessing' }
DruidCogit >> objectRepresentationClass [

	^ self
]
