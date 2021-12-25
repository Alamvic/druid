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
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitive2 [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock |
	self MoveCq: 73 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 |
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMaskInverted [

	| currentBlock jump0 jump1 |
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDivide [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self SubCq: 1 R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra0Reg.
	self SubCq: 1 R: Extra0Reg.
	self
		DivR: Extra0Reg
		R: SendNumArgsReg
		Quo: Extra2Reg
		Rem: Extra1Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self LogicalShiftLeftCq: 3 R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra4Reg.
	self AddCq: 1 R: Extra4Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIsIntegerObject [

	| currentBlock jump0 jump1 |
	self MoveR: ReceiverResultReg R: TempReg.
	self AndCq: 1 R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump0 := self JumpZero: 0.
	self MoveCw: objectMemory trueObject R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
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
DruidTestRTLCompiler >> gen_primitiveMultiplyWithOverflow [

	| currentBlock jump0 jump1 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self LogicalShiftRightCq: 3 R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra0Reg.
	self SubCq: 1 R: Extra0Reg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MulR: Extra0Reg R: Extra1Reg.
	jump0 := self JumpMultiplyNoOverflow: 0.
	self MoveCq: 99 R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: Extra4Reg.
	self AddCq: 1 R: Extra4Reg.
	self MoveR: Extra4Reg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNegated [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self NegateR: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self AddCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOneWithArguments [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 3 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnTrue [

	| currentBlock |
	self MoveCw: objectMemory trueObject R: ReceiverResultReg.
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
DruidTestRTLCompiler >> gen_primitiveSubWithOverflow [

	| currentBlock jump0 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self SubCq: 1 R: SendNumArgsReg.
	self MoveR: TempReg R: Extra0Reg.
	self SubR: SendNumArgsReg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	jump0 := self JumpNoOverflow: 0.
	self MoveCq: 99 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSumWithOverflow [

	| currentBlock jump0 |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self SubCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self AddR: ClassReg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	jump0 := self JumpNoOverflow: 0.
	self MoveCq: 99 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 55 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 55 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]
