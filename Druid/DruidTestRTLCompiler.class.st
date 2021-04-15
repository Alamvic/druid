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
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 73 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
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
	self MoveCq: 42 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra2Reg.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self SubR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self AddR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
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
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
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
	self RetN: 3 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 1 R: TempReg.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra2Reg.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]
