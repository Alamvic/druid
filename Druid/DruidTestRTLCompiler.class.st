Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_assertIsIgnored [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_branchingWithAssigments [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 17 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitive [

	| currentBlock |
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitiveWithArguments [

	| currentBlock |
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
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitiveWithArg [

	| currentBlock jump0 |
	self CmpCq: 0 R: Arg0Reg.
	jump0 := self JumpNonZero: 0.
	self RetN: 2 * objectMemory wordSize.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingSuccess [

	| currentBlock |
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_passingSuccess [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAssertIsIgnored [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveBranchingWithAssigments [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 17 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveByteAt [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveMb: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
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
	self MoveCq: 42 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 |
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 1 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMaskInverted [

	| currentBlock jump0 jump1 |
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 1 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 2 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDNA [

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 1 R: SendNumArgsReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 99 R: Extra0Reg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump2 := self JumpGreater: 0.
	jump3 := self JumpLessOrEqual: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: SendNumArgsReg R: Extra4Reg.
	self MoveCq: 5 R: TempReg.
	self CmpCq: 7 R: ReceiverResultReg.
	self MoveR: Extra4Reg R: TempReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra1Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveCq: 5 R: Extra3Reg.
	self CmpCq: 7 R: ReceiverResultReg.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: Extra1Reg R: SendNumArgsReg.
	self MoveR: Extra3Reg R: Extra4Reg.
	jump5 := self JumpGreater: 0.
	jump6 := self JumpLessOrEqual: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: 99 R: ClassReg.
	self MoveCq: 17 R: Extra0Reg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: Extra0Reg R: Extra4Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	self MoveR: TempReg R: SendNumArgsReg.
	self AddCq: 5 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra4Reg.
	self AddCq: 15 R: Extra4Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump7 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self AddR: Extra4Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self AddCq: 42 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDivide [

	| currentBlock |
	self
		DivR: Arg0Reg
		R: ReceiverResultReg
		Quo: SendNumArgsReg
		Rem: ClassReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIsIntegerObject [

	| currentBlock |
	self MoveCw: objectMemory trueObject R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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
DruidTestRTLCompiler >> gen_primitiveMod [

	| currentBlock |
	self
		DivR: Arg0Reg
		R: ReceiverResultReg
		Quo: ClassReg
		Rem: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveMultiplyWithOverflow [

	| currentBlock jump0 jump1 |
	self MoveR: ReceiverResultReg R: TempReg.
	self LogicalShiftRightCq: 3 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self SubCq: 1 R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MulR: ClassReg R: SendNumArgsReg.
	jump0 := self JumpMultiplyNoOverflow: 0.
	self MoveCq: 99 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self AddCq: 1 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNegated [

	| currentBlock |
	self MoveR: ReceiverResultReg R: ClassReg.
	self NegateR: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNotEqualsThan [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNotEqualsThanInverted [

	| currentBlock jump0 jump1 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 11 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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
DruidTestRTLCompiler >> gen_primitiveSandclock [

	| currentBlock jump0 jump1 jump2 jump3 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 99 R: Extra0Reg.
	self CmpCq: 0 R: ReceiverResultReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	jump2 := self JumpLessOrEqual: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Extra4Reg.
	self AddCq: 5 R: Extra4Reg.
	self MoveR: Extra4Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 116 R: ReceiverResultReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
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

	| currentBlock jump0 jump1 |
	self MoveR: Arg0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ReceiverResultReg R: TempReg.
	self SubR: ClassReg R: TempReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump0 := self JumpNoOverflow: 0.
	self MoveCq: 99 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSumWithOverflow [

	| currentBlock jump0 jump1 |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self AddR: Arg0Reg R: TempReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	jump0 := self JumpNoOverflow: 0.
	self MoveCq: 99 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint16At [

	| currentBlock |
	self MoveCq: 0 R: ClassReg.
	self MoveM16: 0 r: ReceiverResultReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint32At [

	| currentBlock |
	self MoveCq: 0 R: ClassReg.
	self MoveM32: 0 r: ReceiverResultReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint64At [

	| currentBlock |
	self MoveCq: 0 R: ClassReg.
	self MoveM64: 0 r: ReceiverResultReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint8At [

	| currentBlock |
	self MoveCq: 0 R: ClassReg.
	self MoveMb: 0 r: ReceiverResultReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: ReceiverResultReg.
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
	self MoveCq: 42 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]
