Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitive [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitiveWithArguments [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Arg0Reg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitive [

	| currentBlock jump0 |
	currentBlock := self Label.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCq: 73 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 73 R: TempReg.
	self MoveCq: 73 R: TempReg.
	self MoveCq: 73 R: Extra0Reg.
	self MoveCq: 73 R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 73 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: Extra2Reg.
	self MoveR: ClassReg R: TempReg.
	self MoveR: ClassReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self MoveR: ReceiverResultReg R: Extra2Reg.
	self MoveR: Extra0Reg R: TempReg.
	self MoveR: Extra0Reg R: TempReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveR: TempReg R: Extra1Reg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra2Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra1Reg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveR: ReceiverResultReg R: TempReg.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOneWithArguments [

	| currentBlock jump0 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 3 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnTrue [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCw: objectMemory trueObject R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump0 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCw: objectMemory trueObject R: TempReg.
	self MoveCw: objectMemory trueObject R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	self MoveR: TempReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| currentBlock jump0 jump1 jump2 |
	currentBlock := self Label.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: ClassReg.
	self MoveCq: 42 R: Extra0Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 55 R: SendNumArgsReg.
	self MoveCq: 55 R: Extra0Reg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra2Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveCq: 42 R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveCq: 57 R: SendNumArgsReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 jump2 jump3 |
	currentBlock := self Label.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra2Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra1Reg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]
