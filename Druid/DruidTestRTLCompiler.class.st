Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitive [

	| currentBlock |
	currentBlock := self Label.
	currentBlock := self Label.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitiveWithArguments [

	| currentBlock |
	currentBlock := self Label.
	currentBlock := self Label.
	self MoveR: Arg0Reg R: Arg0Reg.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitive [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: false R: TempReg.
	currentBlock := self Label.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	currentBlock := self Label.
	self MoveCq: 73 R: ClassReg.
	currentBlock := self Label.
	self MoveCq: 73 R: ClassReg.
	currentBlock := self Label.
	self MoveCq: 73 R: ClassReg.
	self MoveCq: 73 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	currentBlock := self Label.
	self MoveCq: 0 R: ClassReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra2Reg.
	self MoveR: Extra0Reg R: TempReg.
	self MoveR: Extra0Reg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra1Reg.
	self MoveR: ReceiverResultReg R: Extra2Reg.
	self MoveR: Extra1Reg R: TempReg.
	self MoveR: Extra1Reg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	currentBlock := self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: Extra0Reg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra1Reg.
	self MoveCq: 2 R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: SendNumArgsReg.
	currentBlock := self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self SubCq: 1 R: SendNumArgsReg.
	currentBlock := self Label.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self AddCq: 1 R: SendNumArgsReg.
	currentBlock := self Label.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	currentBlock := self Label.
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOneWithArguments [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 3 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	currentBlock := self Label.
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 3 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnTrue [

	| currentBlock |
	currentBlock := self Label.
	self MoveCq: 1 R: TempReg.
	currentBlock := self Label.
	self MoveCw: objectMemory trueObject R: ClassReg.
	currentBlock := self Label.
	self MoveCw: objectMemory trueObject R: ClassReg.
	currentBlock := self Label.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveCq: 42 R: Extra0Reg.
	self MoveR: Extra0Reg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra1Reg.
	self MoveCq: 55 R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveCq: 42 R: Extra0Reg.
	self MoveR: Extra0Reg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: SendNumArgsReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra1Reg.
	self MoveCq: 57 R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: Extra1Reg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	currentBlock := self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 |
	currentBlock := self Label.
	self MoveCq: 0 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	currentBlock := self Label.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveCq: 42 R: Extra0Reg.
	self MoveR: Extra0Reg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra1Reg.
	self MoveCq: 57 R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: SendNumArgsReg.
	currentBlock := self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]
