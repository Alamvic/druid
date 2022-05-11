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
DruidTestRTLCompiler >> gen_primitiveAt [

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jump9 jump10 jump11 jump12 jump13 jump14 jump15 jump16 jump17 jump18 jump19 jump20 jump21 jump22 jump23 jump24 jump25 jump26 jump27 jump28 jump29 jump30 jump31 jump32 jump33 jump34 jump35 jump36 jump37 jump38 jump39 jump40 jump41 jump42 jump43 jump44 jump45 jump46 jump47 jump48 jump49 |
	self TstCq: 7 R: Arg0Reg.
	jump0 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Arg1Reg.
	self MoveR: Arg1Reg R: Arg0Reg.
	self MoveR: Arg0Reg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self AndCq: 1 R: SendNumArgsReg.
	self CmpCq: 0 R: SendNumArgsReg.
	self OrR: Extra0Reg R: Extra1Reg.
	jump2 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Arg1Reg.
	self MoveR: Arg1Reg R: Arg0Reg.
	self MoveR: Arg0Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: Extra3Reg.
	self LogicalShiftLeftCq: -63 R: Extra3Reg.
	self CmpCq: 1 R: Extra3Reg.
	jump4 := self JumpNonZero: 0.
	self MoveR: Arg0Reg R: ClassReg.
	self LogicalShiftLeftCq: -3 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self AndCq: 16r1FFFFFFFFFFFFFFF R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self SubCq: 16r1FFFFFFFFFFFFFFF R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	self SubCq: 1 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: Extra3Reg.
	self LogicalShiftLeftCq: -3 R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra4Reg.
	self MoveR: Extra4Reg R: TempReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveCq: 0 R: ClassReg.
	self MoveM64: 0 r: Arg0Reg R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self LogicalShiftRightCq: 24 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self AndCq: 31 R: Extra0Reg.
	self MoveR: Arg0Reg R: Extra1Reg.
	self AddCq: 7 R: Extra1Reg.
	self MoveCq: 0 R: Extra3Reg.
	self MoveMb: 0 r: Extra1Reg R: Extra3Reg.
	self CmpCq: 255 R: Extra3Reg.
	jump6 := self JumpNonZero: 0.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self SubCq: 8 R: SendNumArgsReg.
	self MoveCq: 0 R: Extra1Reg.
	self MoveM64: 0 r: SendNumArgsReg R: Extra1Reg.
	self MoveCq: 8 R: Extra4Reg.
	self LogicalShiftLeftR: Extra1Reg R: Extra4Reg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self LogicalShiftRightCq: 8 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Extra4Reg.
	self MoveR: Extra4Reg R: Extra2Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self CmpCq: 5 R: Extra0Reg.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump8 := self JumpLessOrEqual: 0.
	self CmpCq: 16 R: Extra0Reg.
	jump9 := self JumpLess: 0.
	self MoveCq: 3 R: Extra4Reg.
	self LogicalShiftLeftR: Extra2Reg R: Extra4Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	self AndCq: 7 R: Extra1Reg.
	self MoveR: Extra4Reg R: M0.
	self SubR: Extra1Reg R: M0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self CmpCq: 12 R: Extra0Reg.
	jump11 := self JumpLess: 0.
	self MoveCq: 2 R: Extra4Reg.
	self LogicalShiftLeftR: Extra2Reg R: Extra4Reg.
	self MoveR: Extra0Reg R: Extra1Reg.
	self AndCq: 3 R: Extra1Reg.
	self MoveR: Extra4Reg R: Extra4Reg.
	self SubR: Extra1Reg R: Extra4Reg.
	self MoveR: Extra4Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self CmpCq: 10 R: Extra0Reg.
	jump13 := self JumpLess: 0.
	self MoveCq: 1 R: Extra1Reg.
	self LogicalShiftLeftR: Extra2Reg R: Extra1Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self AndCq: 1 R: Extra3Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self SubR: Extra3Reg R: Extra1Reg.
	self MoveR: Extra1Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump14 := self Jump: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: Extra3Reg.
	self CmpCq: 9 R: Extra0Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump15 := self JumpZero: 0.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	jump10 jmpTarget: currentBlock.
	jump12 jmpTarget: currentBlock.
	jump14 jmpTarget: currentBlock.
	jump15 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Arg1Reg R: Extra4Reg.
	self MoveR: Arg0Reg R: Extra1Reg.
	self CmpCq: 9 R: Extra0Reg.
	self CmpCq: 2 R: Extra0Reg.
	self OrR: Extra3Reg R: Extra3Reg.
	jump16 := self JumpZero: 0.
	self MoveCq: 0 R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra1Reg R: SendNumArgsReg.
	self MoveR: Extra4Reg R: Extra2Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump17 := self Jump: 0.
	currentBlock := self Label.
	jump16 jmpTarget: currentBlock.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self CmpCq: 2 R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra1Reg R: SendNumArgsReg.
	self MoveR: Extra4Reg R: Extra2Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump18 := self JumpLess: 0.
	self MoveR: Arg0Reg R: Extra2Reg.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveM64: 0 r: Arg0Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: M0.
	self AndCq: 16r3FFFFF R: M0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self CmpCq: 31 R: M0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump19 := self JumpGreater: 0.
	self CmpCq: 31 R: M0.
	self MoveR: Extra2Reg R: Extra2Reg.
	self MoveR: Extra1Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra4Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump20 := self JumpZero: 0.
	self CmpCq: 8 R: M0.
	jump21 := self JumpNonZero: 0.
	self MoveCw: objectMemory nilObject R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra2Reg.
	self MoveR: Extra1Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra4Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump22 := self Jump: 0.
	currentBlock := self Label.
	jump21 jmpTarget: currentBlock.
	self MoveR: M0 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	currentBlock := self Label.
	jump19 jmpTarget: currentBlock.
	self MoveR: M0 R: M0.
	self LogicalShiftRightCq: 10 R: M0.
	self MoveCw: objectMemory nilObject R: M0.
	self AddCq: 8 R: M0.
	self MoveCq: 3 R: SendNumArgsReg.
	self LogicalShiftLeftR: M0 R: SendNumArgsReg.
	self MoveR: M0 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: SendNumArgsReg.
	self MoveCq: 0 R: M0.
	self MoveM64: 0 r: SendNumArgsReg R: M0.
	self CmpCw: objectMemory nilObject R: M0.
	jump23 := self JumpNonZero: 0.
	self MoveCw: objectMemory nilObject R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump24 := self Jump: 0.
	currentBlock := self Label.
	jump23 jmpTarget: currentBlock.
	self MoveR: M0 R: M0.
	self AndCq: 1023 R: M0.
	self MoveR: M0 R: M0.
	self AddCq: 8 R: M0.
	self MoveCq: 3 R: SendNumArgsReg.
	self LogicalShiftLeftR: M0 R: SendNumArgsReg.
	self MoveR: M0 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: SendNumArgsReg.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveM64: 0 r: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	currentBlock := self Label.
	jump24 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Arg1Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra2Reg.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	currentBlock := self Label.
	jump20 jmpTarget: currentBlock.
	jump22 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self AddCq: 8 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self AddCq: 16 R: Extra2Reg.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveM64: 0 r: Extra2Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self LogicalShiftLeftCq: -63 R: Extra2Reg.
	self CmpCq: 1 R: Extra2Reg.
	jump25 := self JumpNonZero: 0.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self LogicalShiftLeftCq: -3 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self AndCq: 16r1FFFFFFFFFFFFFFF R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self SubCq: 16r1FFFFFFFFFFFFFFF R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self SubCq: 1 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	jump26 := self Jump: 0.
	currentBlock := self Label.
	jump25 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self LogicalShiftLeftCq: -3 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump26 jmpTarget: currentBlock.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: Extra2Reg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self AndCq: 16rFFFF R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra2Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	currentBlock := self Label.
	jump17 jmpTarget: currentBlock.
	jump18 jmpTarget: currentBlock.
	self MoveCq: 0 R: Extra4Reg.
	self CmpCq: 3 R: Extra0Reg.
	self MoveR: ClassReg R: ClassReg.
	self AndCq: 16r3FFFFF R: ClassReg.
	self CmpCq: 36 R: ClassReg.
	self AndR: Extra1Reg R: Extra1Reg.
	self MoveR: Extra4Reg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self MoveR: Extra4Reg R: SendNumArgsReg.
	jump27 := self JumpNonZero: 0.
	self MoveR: M0 R: Extra1Reg.
	self SubR: M0 R: Extra1Reg.
	self CmpCq: 24 R: Extra0Reg.
	jump28 := self JumpLess: 0.
	self MoveCq: 0 R: ClassReg.
	self MoveR: ClassReg R: Extra3Reg.
	jump29 := self Jump: 0.
	currentBlock := self Label.
	jump28 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	currentBlock := self Label.
	jump29 jmpTarget: currentBlock.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveCq: 0 R: ClassReg.
	self CmpR: Extra3Reg R: TempReg.
	self CmpR: Extra1Reg R: TempReg.
	self AndR: Extra3Reg R: Extra3Reg.
	jump30 := self JumpZero: 0.
	self MoveR: TempReg R: M0.
	self AddR: M0 R: M0.
	self CmpCq: 5 R: Extra0Reg.
	jump31 := self JumpLessOrEqual: 0.
	jump32 := self JumpGreater: 0.
	currentBlock := self Label.
	jump30 jmpTarget: currentBlock.
	self CmpCq: 1 R: Extra0Reg.
	jump33 := self JumpLessOrEqual: 0.
	jump34 := self JumpGreater: 0.
	currentBlock := self Label.
	jump31 jmpTarget: currentBlock.
	self MoveR: M0 R: Extra1Reg.
	self SubCq: 1 R: Extra1Reg.
	self MoveR: Arg0Reg R: TempReg.
	self AddCq: 8 R: TempReg.
	self MoveCq: 3 R: Extra1Reg.
	self LogicalShiftLeftR: Extra1Reg R: Extra1Reg.
	self MoveR: TempReg R: Extra1Reg.
	self AddR: Extra1Reg R: Extra1Reg.
	self MoveCq: 0 R: TempReg.
	self MoveM64: 0 r: Extra1Reg R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump35 := self Jump: 0.
	currentBlock := self Label.
	jump32 jmpTarget: currentBlock.
	self CmpCq: 16 R: Extra0Reg.
	jump36 := self JumpGreaterOrEqual: 0.
	jump37 := self JumpLess: 0.
	currentBlock := self Label.
	jump33 jmpTarget: currentBlock.
	self MoveCq: 2 R: TempReg.
	self MoveR: TempReg R: Extra1Reg.
	jump38 := self Jump: 0.
	currentBlock := self Label.
	jump34 jmpTarget: currentBlock.
	self MoveCq: 4 R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	jump39 := self Jump: 0.
	currentBlock := self Label.
	jump36 jmpTarget: currentBlock.
	self MoveR: M0 R: M0.
	self SubCq: 1 R: M0.
	self MoveR: Arg0Reg R: Extra1Reg.
	self AddCq: 8 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self AddR: M0 R: Extra1Reg.
	self MoveCq: 0 R: Extra1Reg.
	self MoveMb: 0 r: Extra1Reg R: Extra1Reg.
	self MoveCq: 3 R: Extra1Reg.
	self LogicalShiftLeftR: Extra1Reg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self AddCq: 1 R: Extra1Reg.
	self MoveR: Extra1Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump40 := self Jump: 0.
	currentBlock := self Label.
	jump37 jmpTarget: currentBlock.
	self CmpCq: 12 R: Extra0Reg.
	jump41 := self JumpGreaterOrEqual: 0.
	jump42 := self JumpLess: 0.
	currentBlock := self Label.
	jump38 jmpTarget: currentBlock.
	jump39 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveCq: 0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: TempReg R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: Extra1Reg R: Extra1Reg.
	jump43 := self Jump: 0.
	currentBlock := self Label.
	jump41 jmpTarget: currentBlock.
	self MoveR: M0 R: M0.
	self SubCq: 1 R: M0.
	self MoveR: Arg0Reg R: M0.
	self AddCq: 8 R: M0.
	self MoveCq: 1 R: Extra3Reg.
	self LogicalShiftLeftR: M0 R: Extra3Reg.
	self MoveR: M0 R: Extra3Reg.
	self AddR: Extra3Reg R: Extra3Reg.
	self MoveCq: 0 R: Extra3Reg.
	self MoveM16: 0 r: Extra3Reg R: Extra3Reg.
	self MoveCq: 3 R: Extra3Reg.
	self LogicalShiftLeftR: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	self AddCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump44 := self Jump: 0.
	currentBlock := self Label.
	jump42 jmpTarget: currentBlock.
	self CmpCq: 9 R: Extra0Reg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	jump45 := self JumpZero: 0.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveCq: 0 R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	currentBlock := self Label.
	jump35 jmpTarget: currentBlock.
	jump40 jmpTarget: currentBlock.
	jump44 jmpTarget: currentBlock.
	jump45 jmpTarget: currentBlock.
	self MoveCq: 0 R: TempReg.
	self MoveR: TempReg R: MEMORY0.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: M0 R: TempReg.
	self MoveR: TempReg R: Extra1Reg.
	currentBlock := self Label.
	jump43 jmpTarget: currentBlock.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: TempReg R: TempReg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveR: ClassReg R: Extra0Reg.
	self MoveR: Extra1Reg R: SendNumArgsReg.
	currentBlock := self Label.
	jump27 jmpTarget: currentBlock.
	self CmpCq: 0 R: SendNumArgsReg.
	jump46 := self JumpNonZero: 0.
	self MoveR: Extra3Reg R: Arg1Reg.
	self MoveR: Extra2Reg R: Arg0Reg.
	self MoveR: TempReg R: ReceiverResultReg.
	self CmpCq: 0 R: SendNumArgsReg.
	jump47 := self JumpZero: 0.
	jump48 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump46 jmpTarget: currentBlock.
	self MoveR: TempReg R: Arg1Reg.
	self MoveR: Extra2Reg R: Arg0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self CmpCq: 0 R: SendNumArgsReg.
	jump49 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump47 jmpTarget: currentBlock.
	self RetN: 3 * objectMemory wordSize.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump3 jmpTarget: currentBlock.
	jump48 jmpTarget: currentBlock.
	jump49 jmpTarget: currentBlock.
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

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 1 R: TempReg.
	self MoveR: TempReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 99 R: TempReg.
	self MoveR: TempReg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self CmpCq: 0 R: ReceiverResultReg.
	jump2 := self JumpLessOrEqual: 0.
	self MoveCq: 5 R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 17 R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self CmpCq: 7 R: ReceiverResultReg.
	jump4 := self JumpLessOrEqual: 0.
	self AddR: ClassReg R: TempReg.
	self AddCq: 15 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self AddR: ClassReg R: TempReg.
	self AddCq: 42 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
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
	self
		DivR: Arg0Reg
		R: ReceiverResultReg
		Quo: TempReg
		Rem: ClassReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
	self MoveCw: objectMemory trueObject R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCw: objectMemory falseObject R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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
		Rem: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
	self MulR: ClassReg R: TempReg.
	jump0 := self JumpMultiplyNoOverflow: 0.
	self MoveCq: 99 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNegated [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self NegateR: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
	self MoveCq: 1 R: TempReg.
	self MoveR: TempReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 99 R: TempReg.
	self MoveR: TempReg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self CmpCq: 0 R: ReceiverResultReg.
	jump2 := self JumpLessOrEqual: 0.
	self AddCq: 5 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self AddCq: 17 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
	self MoveR: ReceiverResultReg R: ClassReg.
	self SubR: TempReg R: ClassReg.
	jump0 := self JumpNoOverflow: 0.
	self MoveCq: 99 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
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
	self MoveR: Arg0Reg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	jump0 := self JumpNoOverflow: 0.
	self MoveCq: 99 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self RetN: 2 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint16At [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveM16: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint32At [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveM32: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint64At [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveM64: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self RetN: 1 * objectMemory wordSize.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint8At [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveMb: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
