Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitive [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitiveWithArguments [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveR: Arg0Reg R: Arg0Reg.
	self MoveR: ReceiverResultReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitive [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: false asBit R: ClassReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 73 R: SendNumArgsReg.
	self MoveCq: 73 R: Extra0Reg.
	self MoveCq: 73 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 0 R: SendNumArgsReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: Extra1Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Extra1Reg R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra2Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Extra2Reg R: ClassReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 1 R: Extra0Reg.
	self MoveCq: 1 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra2Reg.
	self MoveCq: 2 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMaskInverted [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 1 R: Extra0Reg.
	self MoveCq: 1 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra2Reg.
	self MoveCq: 2 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self SubCq: 1 R: Extra0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThan [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThan [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThanInverted [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self AddCq: 1 R: Extra0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIsIntegerObject [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveCq: 1 R: Extra0Reg.
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCw: objectMemory trueObject R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ClassReg R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra2Reg.
	self MoveR: Extra1Reg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra2Reg.
	self MoveR: Extra1Reg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThan [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThan [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThanInverted [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThanThanInverted [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOneWithArguments [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 3 R: ClassReg.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 3 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnTrue [

	| currentBlock |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 1 R: ClassReg.
	self MoveCw: objectMemory trueObject R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSmallOrEqualsThan [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSmallThan [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 10 R: ClassReg.
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 5 R: Extra0Reg.
	self MoveCq: 5 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra2Reg.
	self MoveCq: 11 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 0 R: ClassReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 1 R: Extra0Reg.
	self MoveCq: 42 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra2Reg.
	self MoveCq: 55 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 0 R: ClassReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: Extra0Reg.
	self MoveCq: 42 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: Extra0Reg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra2Reg.
	self MoveCq: 57 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra1Reg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 |
	self MoveCq: true asBit R: TempReg.
	self MoveCq: 0 R: ClassReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: Extra0Reg.
	self MoveCq: 42 R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra2Reg.
	self MoveCq: 57 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra0Reg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]
