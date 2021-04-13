Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 73 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| currentBlock jump0 jump1 jump2 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 0 R: ClassReg.
	self CmpR: ReceiverResultReg R: ClassReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: TempReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveR: TempReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 jump2 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: SendNumArgsReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self MoveR: ClassReg R: TempReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 jump2 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 1 R: TempReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize
]
