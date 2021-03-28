Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 73 R: ReceiverResultReg.
	self RetN: 8
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: ReceiverResultReg.
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
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: ClassReg.
	self MoveR: ClassReg R: Extra0Reg.
	self MoveR: ClassReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 8
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 jump2 |
	currentBlock := self Label.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ClassReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 8
]
