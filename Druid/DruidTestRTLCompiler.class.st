Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAnd [

	| jumpNext jumpTrue jump1 currentBlock jump2 |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 0 R: TempReg.
	jump2 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 10 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAsInteger [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveBitShift [

	| jump1 jump2 currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveR: Arg0Reg R: TempReg.
	self NegateR: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self ArithmeticShiftRightR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveR: Arg0Reg R: TempReg.
	self LogicalShiftLeftR: TempReg R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDivide [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self
		DivR: ClassReg
		R: TempReg
		Quo: TempReg
		Rem: ClassReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveImplicitArgumentBitShiftLeft [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self NegateR: TempReg.
	self MoveCq: 1 R: ClassReg.
	self ArithmeticShiftRightR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: TempReg.
	self LogicalShiftLeftR: TempReg R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIntegerDivideByConstant [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveCq: 2 R: ClassReg.
	self
		DivR: ClassReg
		R: TempReg
		Quo: TempReg
		Rem: ClassReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIntegerRawBitsAsFloat [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg Rd: DPFPReg0.
	self MoveRd: DPFPReg0 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIsIntegerObject [

	| jumpNext jumpTrue jump1 currentBlock jump2 |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 1 R: TempReg.
	jumpTrue := self JumpNonZero: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: objectMemory falseObject R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: objectMemory trueObject R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveModByConstant [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveCq: 2 R: ClassReg.
	self
		DivR: ClassReg
		R: TempReg
		Quo: TempReg
		Rem: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithNonCompilableBranch [

	| jump1 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]
