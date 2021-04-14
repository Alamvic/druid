Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 73 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 0 R: ClassReg.
	self CmpR: ReceiverResultReg R: ClassReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: TempReg.
	self MoveR: TempReg R: SendNumArgsReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOneWithArguments [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveCq: 3 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self RetN: 3 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
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
	self MoveCq: 1 R: TempReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: TempReg.
	self MoveR: TempReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 1 R: ClassReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize
]
