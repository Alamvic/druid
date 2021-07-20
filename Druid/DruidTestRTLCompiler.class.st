Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitive [

	| currentBlock |
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitiveWithArguments [

	| currentBlock |
	self MoveR: Arg0Reg R: Arg0Reg.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitive [

	| currentBlock |
	self MoveCq: false R: TempReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveCq: 73 R: ClassReg.
	self MoveCq: 73 R: ClassReg.
	self MoveCq: 73 R: ClassReg.
	self MoveCq: 73 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| currentBlock jump0 jump1 |
	self MoveCq: 1 R: TempReg.
	self MoveCq: 0 R: ClassReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
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
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 |
	self MoveCq: 1 R: TempReg.
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
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
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self SubCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self AddCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOneWithArguments [

	| currentBlock |
	self MoveCq: 3 R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 3 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnTrue [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveCw: objectMemory trueObject R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| currentBlock jump0 jump1 |
	self MoveCq: 0 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
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
	self MoveCq: 0 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
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
	self MoveCq: 1 R: Extra2Reg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 |
	self MoveCq: 0 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
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
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]
