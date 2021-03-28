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
	self RetN: 8
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
	self MoveCq: 42 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	self RetN: 8
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 8
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithConditionalFlow [

	| currentBlock jump0 jump1 jump2 |
	currentBlock := self Label.
	self CmpR: ReceiverResultReg R: 0.
	jump0 := self JumpZero: 0.
	currentBlock := self Label.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: Phixx R: ReceiverResultReg.
	self RetN: 8
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
	self MoveCq: 1 R: ClassReg.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	self RetN: 8
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
	self RetN: 8
]
