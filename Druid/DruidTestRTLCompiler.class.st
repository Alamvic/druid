Class {
	#name : #DruidTestRTLCompiler,
	#superclass : #StackToRegisterMappingCogit,
	#category : #'Druid-Tests'
}

{ #category : #deployment }
DruidTestRTLCompiler class >> replaceMethodOnJIT: selector [

| druidMethod cogMethod |
druidMethod := DruidTestRTLCompiler lookupSelector: (#gen_, selector).
cogMethod := druidMethod ast copy.

"Change selector"
cogMethod selector: (#gen, selector capitalized).

"Replace self by cogit variable"
cogMethod nodesDo: [ :n | n isSelfVariable ifTrue: [ n replaceWith: (RBVariableNode named: 'cogit') ] ].

"Add type anotations"
cogMethod pragmas: { }.
cogMethod temporaries do: [ :temp | |args|	
	args := {temp name. #'AbstractInstruction *'} collect: [ :arg | RBLiteralNode value: arg ].
	cogMethod addPragma: (RBPragmaNode selector: #var:type: arguments: args).	
].


CogObjectRepresentation compile: cogMethod formattedCode classified: 'primitive generators'
]

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
DruidTestRTLCompiler >> gen_bytecodePrimAdd [

	| jump1 jump2 currentBlock |
	(self ssDescriptorAt: 1) moveToReg: TempReg.
	(self ssDescriptorAt: 0) moveToReg: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self AndR: ClassReg R: SendNumArgsReg.
	self marshallSendArguments: 1.
	self TstCq: 1 R: SendNumArgsReg.
	jump1 := self JumpZero: 0.
	self SubCq: 1 R: TempReg.
	self AddR: ClassReg R: TempReg.
	jump2 := self JumpNoOverflow: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self
		genMarshalledSend:  -1
		numArgs: 1
		sendTable: ordinarySendTrampolines.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self ssPushRegister: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_duplicateTopBytecode [

	| currentBlock |
	(self ssDescriptorAt: 0) moveToReg: TempReg.
	self ssPushRegister: TempReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitive [

	| currentBlock |
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_emptyPrimitiveWithArguments [

	| currentBlock |
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitive [

	| currentBlock |
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitive2 [

	| currentBlock |
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingPrimitiveWithArg [

	| currentBlock |
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_failingSuccess [

	| currentBlock |
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_ifZeroFailPrimitive [

	| jump1 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_passingSuccess [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAccessTempAfterCall [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAdd [

	| jump1 jump2 currentBlock |
	self mclassIsSmallInteger ifFalse: [ ^ UnimplementedPrimitive ].
	self TstCq: 1 R: Arg0Reg.
	jump1 := self JumpZero: 0.
	self MoveR: Arg0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	jump2 := self JumpOverflow: 0.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAnd [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump2 := self JumpGreater: 0.
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
DruidTestRTLCompiler >> gen_primitiveArithmeticBitShiftRight [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self ArithmeticShiftRightR: ClassReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAssertDeferredInline [

	| jump1 jump2 jump3 currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveR: TempReg Aw: coInterpreter primitiveFailureCodeAddress.
	self MoveAw: coInterpreter primitiveFailureCodeAddress R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self CmpCq: 0 R: ClassReg.
	jump1 := self JumpZero: 0.
	self MoveR: TempReg Aw: coInterpreter primitiveFailureCodeAddress.
	self MoveAw: coInterpreter primitiveFailureCodeAddress R: ClassReg.
	self MoveCq: 42 R: ReceiverResultReg.
	self CmpCq: 0 R: ClassReg.
	jump2 := self JumpZero: 0.
	jump3 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: TempReg Aw: coInterpreter primitiveFailureCodeAddress.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ClassReg Aw: coInterpreter primitiveFailureCodeAddress.
	self MoveAw: coInterpreter primitiveFailureCodeAddress R: ClassReg.
	self MoveR: ClassReg Aw: coInterpreter primitiveFailureCodeAddress.
	self MoveAw: coInterpreter primitiveFailureCodeAddress R: ClassReg.
	self MoveCq: 42 R: ReceiverResultReg.
	self CmpCq: 0 R: ClassReg.
	jump1 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAssertIsIgnored [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAt [

	| jump1 jump10 jumpNext jump9 jump6 jump3 currentBlock jump8 jump5 jumpTrue jump2 jump7 jump4 |
	self AddCq: 8 R: SPReg.
	self TstCq: 7 R: ReceiverResultReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 1 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self AndR: ClassReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: ClassReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: ClassReg.
	jumpNext jmpTarget: self Label.
	self MoveCq: 0 R: SendNumArgsReg.
	self CmpR: TempReg R: SendNumArgsReg.
	jump2 := self JumpZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveR: ClassReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self CmpCq: 0 R: SendNumArgsReg.
	jump3 := self JumpNonZero: 0.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self ArithmeticShiftRightCq: 3 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveM64: 0 r: ReceiverResultReg R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self ArithmeticShiftRightCq: 24 R: TempReg.
	self AndCq: 31 R: TempReg.
	self MoveCq: 7 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self AddR: Extra1Reg R: Extra0Reg.
	self MoveMb: 0 r: Extra0Reg R: Extra1Reg.
	self AndCq: 255 R: Extra1Reg.
	self CmpCq: 255 R: Extra1Reg.
	jump2 := self JumpZero: 0.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self SubCq: 8 R: Extra1Reg.
	self MoveM64: 0 r: Extra1Reg R: Extra1Reg.
	self LogicalShiftLeftCq: 8 R: Extra1Reg.
	self ArithmeticShiftRightCq: 8 R: Extra1Reg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self CmpCq: 5 R: TempReg.
	jump4 := self JumpLessOrEqual: 0.
	self CmpCq: 16 R: TempReg.
	jump2 := self JumpGreaterOrEqual: 0.
	self CmpCq: 12 R: TempReg.
	jump5 := self JumpGreaterOrEqual: 0.
	self CmpCq: 10 R: TempReg.
	jump6 := self JumpGreaterOrEqual: 0.
	self CmpCq: 9 R: TempReg.
	jump7 := self JumpZero: 0.
	self MoveCq: 0 R: Extra1Reg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 1 R: Extra1Reg.
	self MoveR: TempReg R: Extra0Reg.
	self AndCq: 1 R: Extra0Reg.
	self SubR: Extra0Reg R: Extra1Reg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	jump7 jmpTarget: currentBlock.
	self CmpCq: 9 R: TempReg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra0Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra0Reg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 9 R: TempReg.
	jump7 := self JumpGreaterOrEqual: 0.
	self MoveR: Extra1Reg R: Extra0Reg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 2 R: Extra1Reg.
	self MoveR: TempReg R: Extra0Reg.
	self AndCq: 3 R: Extra0Reg.
	self SubR: Extra0Reg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra0Reg.
	self CmpCq: 9 R: TempReg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra2Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra2Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra0Reg R: Extra1Reg.
	self MoveR: Extra2Reg R: Extra0Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: Extra2Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 3 R: Extra1Reg.
	self MoveR: TempReg R: Extra0Reg.
	self AndCq: 7 R: Extra0Reg.
	self SubR: Extra0Reg R: Extra1Reg.
	self CmpCq: 9 R: TempReg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra0Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra0Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra1Reg R: Extra2Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra1Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: Extra0Reg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self CmpCq: 2 R: TempReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: Extra2Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra2Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra2Reg R: Extra1Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra1Reg.
	jump7 := self JumpNonZero: 0.
	self CmpCq: 2 R: TempReg.
	jump8 := self JumpLess: 0.
	self MoveM64: 0 r: ReceiverResultReg R: Extra0Reg.
	self AndCq: 16r3FFFFF R: Extra0Reg.
	self CmpCq: 31 R: Extra0Reg.
	jump4 := self JumpGreater: 0.
	self CmpCq: 31 R: Extra0Reg.
	jump2 := self JumpZero: 0.
	self CmpCq: 8 R: Extra0Reg.
	jump5 := self JumpZero: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra1Reg.
	self ArithmeticShiftRightCq: 10 R: Extra1Reg.
	self MoveCq: 8 R: SendNumArgsReg.
	self AddCq: objectMemory nilObject R: SendNumArgsReg.
	self LogicalShiftLeftCq: 3 R: Extra1Reg.
	self AddR: Extra1Reg R: SendNumArgsReg.
	self MoveM64: 0 r: SendNumArgsReg R: SendNumArgsReg.
	self CmpCq: objectMemory nilObject R: SendNumArgsReg.
	jump4 := self JumpZero: 0.
	self AndCq: 1023 R: Extra0Reg.
	self AddCq: 8 R: SendNumArgsReg.
	self LogicalShiftLeftCq: 3 R: Extra0Reg.
	self AddR: Extra0Reg R: SendNumArgsReg.
	self MoveM64: 0 r: SendNumArgsReg R: SendNumArgsReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveCq: objectMemory nilObject R: SendNumArgsReg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveCq: objectMemory nilObject R: SendNumArgsReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	self AddCq: 8 R: SendNumArgsReg.
	self AddCq: 16 R: SendNumArgsReg.
	self MoveM64: 0 r: SendNumArgsReg R: SendNumArgsReg.
	self ArithmeticShiftRightCq: 3 R: SendNumArgsReg.
	self AndCq: 16rFFFF R: SendNumArgsReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveCq: 0 R: SendNumArgsReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	self CmpCq: 3 R: TempReg.
	jump8 := self JumpZero: 0.
	self MoveCq: 0 R: ClassReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self AndCq: 16r3FFFFF R: ClassReg.
	self CmpCq: 36 R: ClassReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: ClassReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: ClassReg.
	jumpNext jmpTarget: self Label.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self CmpCq: 0 R: ClassReg.
	jump5 := self JumpNonZero: 0.
	self SubR: SendNumArgsReg R: Extra2Reg.
	self CmpCq: 24 R: TempReg.
	jump8 := self JumpGreaterOrEqual: 0.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self CmpCq: 1 R: Extra3Reg.
	jump7 := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: ClassReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self CmpR: Extra2Reg R: Extra3Reg.
	jumpTrue := self JumpLessOrEqual: 0.
	self MoveCq: 0 R: ClassReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: ClassReg.
	jumpNext jmpTarget: self Label.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self CmpCq: 0 R: ClassReg.
	jump6 := self JumpZero: 0.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self AddR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self CmpCq: 5 R: TempReg.
	jump7 := self JumpLessOrEqual: 0.
	self CmpCq: 16 R: TempReg.
	jump2 := self JumpGreaterOrEqual: 0.
	self CmpCq: 12 R: TempReg.
	jump4 := self JumpGreaterOrEqual: 0.
	self CmpCq: 9 R: TempReg.
	jump9 := self JumpZero: 0.
	self MoveCq: 0 R: ReceiverResultReg.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self SubCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveCq: 8 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: ClassReg.
	self MoveM16: 0 r: ClassReg R: SendNumArgsReg.
	self AndCq: 16rFFFF R: SendNumArgsReg.
	self LogicalShiftLeftCq: 3 R: SendNumArgsReg.
	self AddCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self SubCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveCq: 8 R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: SendNumArgsReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: SendNumArgsReg.
	self MoveMb: 0 r: SendNumArgsReg R: ClassReg.
	self AndCq: 255 R: ClassReg.
	self LogicalShiftLeftCq: 3 R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self SubCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveCq: 8 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 3 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: ClassReg.
	self MoveM64: 0 r: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self SubCq: 8 R: SPReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump3 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	jump9 jmpTarget: currentBlock.
	self SubCq: 8 R: SPReg.
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
	self MoveR: ClassReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveBitXor [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self XorR: ClassReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveBranchingWithAssigments [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
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
DruidTestRTLCompiler >> gen_primitiveCallingBlockReadingExternalTemp [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingBlockReturningArgument [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingBlockReturningConstant [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingBlockWithNonLocalReturn [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingBlockWritingExternalTemp [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingEmptyBlock [

	| currentBlock |
	self MoveCq: 18 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodReturningConstant [

	| currentBlock |
	self MoveCq: 73 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCallingMethodWithEarlyReturn [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 57 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCascadedUint16AtPut [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveR: TempReg M16: 0 r: ReceiverResultReg.
	self MoveCq: 2 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	self MoveCq: 2 R: ClassReg.
	self MoveR: ClassReg M16: 0 r: TempReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveCaseOfOtherwiseValue [

	| jump1 jump2 jump3 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 1 R: TempReg.
	jump2 := self JumpZero: 0.
	self MoveCq: 77 R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 57 R: ReceiverResultReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveClassVariable [

	| currentBlock |
	self MoveCq: 34 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveClassVariableWithBranch [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 36 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 35 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| jump1 jump2 currentBlock |
	self TstCq: 1 R: ReceiverResultReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 2 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMaskInverted [

	| jump1 jump2 currentBlock |
	self TstCq: 1 R: ReceiverResultReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 2 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithObjectReference [

	| jump1 jump2 currentBlock |
	self MoveCq: objectMemory trueObject R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 2 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDNA [

	| jump1 jump2 jump3 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 99 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self CmpCq: 0 R: ClassReg.
	jump2 := self JumpGreater: 0.
	self MoveCq: 99 R: ClassReg.
	self MoveCq: 17 R: TempReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 5 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self CmpCq: 7 R: SendNumArgsReg.
	jump2 := self JumpGreater: 0.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: SendNumArgsReg R: TempReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self AddR: TempReg R: ClassReg.
	self AddCq: 42 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: ClassReg.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: ClassReg R: TempReg.
	self MoveR: SendNumArgsReg R: ClassReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self AddCq: 5 R: TempReg.
	self AddCq: 15 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDeadBranchWithError [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrement [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrementConstantArgument [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDecrementConstantReceiver [

	| currentBlock |
	self MoveCq: 42 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self SubR: ClassReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
		Quo: ClassReg
		Rem: TempReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDivideByConstant [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveCq: 2 R: ClassReg.
	self
		DivR: ClassReg
		R: TempReg
		Quo: ClassReg
		Rem: TempReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDoubleDeferredInline [

	| jump1 currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveR: TempReg Aw: coInterpreter primitiveFailureCodeAddress.
	self MoveAw: coInterpreter primitiveFailureCodeAddress R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveCq: 2 R: SendNumArgsReg.
	self MulR: ClassReg R: SendNumArgsReg.
	self MoveR: TempReg Aw: coInterpreter primitiveFailureCodeAddress.
	self MoveAw: coInterpreter primitiveFailureCodeAddress R: ClassReg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self CmpCq: 0 R: ClassReg.
	jump1 := self JumpNonZero: 0.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThan [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThanFloats [

	| jumpNext currentBlock jumpTrue |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self CmpRd: DPFPReg1 Rd: DPFPReg0.
	jumpTrue := self JumpFPEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThanInverted [

	| jump1 jump2 currentBlock |
	self MoveCq: 10 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThanReferenceValue [

	| jumpNext currentBlock jumpTrue |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCw: objectMemory trueObject R: TempReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThanValue [

	| jumpNext currentBlock jumpTrue |
	self MoveCq: 0 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self CmpR: ClassReg R: TempReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: ClassReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: ClassReg.
	jumpNext jmpTarget: self Label.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFalseAsInteger [

	| currentBlock |
	self MoveCq: 0 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFalseAssignmentAsInteger [

	| currentBlock |
	self MoveCq: 0 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFalseNot [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFloat64At [

	| currentBlock |
	self MoveM64: 0 r: ReceiverResultReg Rd: DPFPReg0.
	self MoveRd: DPFPReg0 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFloat64AtPut [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveRd: DPFPReg0 M64: 0 r: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFloatAdd [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self AddRd: DPFPReg1 Rd: DPFPReg0.
	self MoveRd: DPFPReg0 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFloatDivide [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self DivRd: DPFPReg1 Rd: DPFPReg0.
	self MoveRd: DPFPReg0 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFloatMultiply [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self MulRd: DPFPReg0 Rd: DPFPReg1.
	self MoveRd: DPFPReg1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveFloatSubtract [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self SubRd: DPFPReg1 Rd: DPFPReg0.
	self MoveRd: DPFPReg0 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualThanValue [

	| jumpNext currentBlock jumpTrue |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThan [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpGreaterOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThanFloats [

	| jumpNext currentBlock jumpTrue |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self CmpRd: DPFPReg1 Rd: DPFPReg0.
	jumpTrue := self JumpFPGreaterOrEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThanInverted [

	| jump1 jump2 currentBlock |
	self MoveCq: 10 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump1 := self JumpGreaterOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThan [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThanFloats [

	| jumpNext currentBlock jumpTrue |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self CmpRd: DPFPReg1 Rd: DPFPReg0.
	jumpTrue := self JumpFPGreater: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThanInverted [

	| jump1 jump2 currentBlock |
	self MoveCq: 10 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThanReferenceValue [

	| jumpNext currentBlock jumpTrue |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: objectMemory trueObject R: TempReg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIfFalseIfTrueReturningValue [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 57 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIfTrueIfFalseAssigningValue [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 57 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIfTrueIfFalseReturningValue [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 57 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIfTrueStatement [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 10 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
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
	self MoveR: ClassReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveImplicitBitShiftLeft [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self LogicalShiftLeftCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveImplicitBitShiftRight [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self ArithmeticShiftRightCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrementConstantArgument [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIncrementConstantReceiver [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIntegerAsFloat [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveRd: DPFPReg0 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIntegerDivide [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self
		DivR: ClassReg
		R: TempReg
		Quo: ClassReg
		Rem: TempReg.
	self MoveR: ClassReg R: ReceiverResultReg.
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
		Quo: ClassReg
		Rem: TempReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIntegerRawBitsAsFloat [

	| currentBlock |
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIsIntegerObject [

	| jump1 jump2 currentBlock |
	self TstCq: 1 R: ReceiverResultReg.
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
DruidTestRTLCompiler >> gen_primitiveJITCompileTimeExpression [

	| currentBlock |
	self MoveCq: 5 R: TempReg.
	self AddCq: objectMemory trueObject R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualThanValue [

	| jumpNext currentBlock jumpTrue |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jumpTrue := self JumpLessOrEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThan [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThanFloats [

	| jumpNext currentBlock jumpTrue |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self CmpRd: DPFPReg1 Rd: DPFPReg0.
	jumpTrue := self JumpFPLessOrEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThanInverted [

	| jump1 jump2 currentBlock |
	self MoveCq: 10 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThan [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpLess: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThanFloats [

	| jumpNext currentBlock jumpTrue |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self CmpRd: DPFPReg1 Rd: DPFPReg0.
	jumpTrue := self JumpFPLess: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThanInverted [

	| jump1 jump2 currentBlock |
	self MoveCq: 10 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump1 := self JumpLess: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
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
DruidTestRTLCompiler >> gen_primitiveLessThanValue [

	| jumpNext currentBlock jumpTrue |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jumpTrue := self JumpLess: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLoopIncrementing [

	| jump1 currentBlock b19 |
	self MoveCq: 10 R: TempReg.
	b19 := self Label.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self CmpCq: 15 R: ClassReg.
	jump1 := self JumpLess: 0.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ClassReg R: TempReg.
	jump1 := self Jump: b19.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLoopIncrementingGlobalState [

	| jump1 currentBlock b19 |
	self MoveCq: 10 R: TempReg.
	b19 := self Label.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self CmpCq: 15 R: ClassReg.
	jump1 := self JumpLess: 0.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ClassReg R: TempReg.
	jump1 := self Jump: b19.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLoopWithInvariant [

	| jump1 currentBlock b14 |
	self MoveCq: 0 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	b14 := self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra0Reg.
	self MulR: SendNumArgsReg R: Extra0Reg.
	self MoveR: TempReg R: SendNumArgsReg.
	self AddR: Extra0Reg R: SendNumArgsReg.
	self CmpCq: 15 R: SendNumArgsReg.
	jump1 := self JumpLess: 0.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: TempReg.
	jump1 := self Jump: b14.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveMod [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self
		DivR: ClassReg
		R: TempReg
		Quo: ClassReg
		Rem: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
		Quo: ClassReg
		Rem: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveMultiply [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self MulR: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveMultiplyConstantArgument [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveCq: 2 R: ClassReg.
	self MulR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveMultiplyConstantReceiver [

	| currentBlock |
	self MoveCq: 2 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MulR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveMultiplyInverted [

	| currentBlock |
	self MoveCq: 2 R: TempReg.
	self MoveR: TempReg Cq: 2.
	self MulCq: 2 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveMultiplyWithOverflow [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self ArithmeticShiftRightCq: 3 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self SubCq: 1 R: ClassReg.
	self MulR: ClassReg R: TempReg.
	jump1 := self JumpMultiplyOverflow: 0.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 99 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNegated [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self NegateR: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNew [

	| jump1 jumpNext b491 jump6 jump3 currentBlock jump8 jump5 jumpTrue jump2 jump7 jump4 |
	self MoveCq: 8 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	self AddCq: 16 R: TempReg.
	self MoveM64: 0 r: TempReg R: ClassReg.
	self ArithmeticShiftRightCq: 3 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self ArithmeticShiftRightCq: 16 R: TempReg.
	self AndCq: 31 R: TempReg.
	self CmpCq: 1 R: TempReg.
	jumpTrue := self JumpLessOrEqual: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 1 R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	self CmpCq: 5 R: TempReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	jumpNext jmpTarget: self Label.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self CmpCq: 0 R: SendNumArgsReg.
	jump2 := self JumpNonZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: TempReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 4 R: SendNumArgsReg.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self AddR: Extra0Reg R: SendNumArgsReg.
	self MoveM32: 0 r: SendNumArgsReg R: Extra0Reg.
	self AndCq: 16rFFFFFFFF R: Extra0Reg.
	self AndCq: 16r3FFFFF R: Extra0Reg.
	self CmpCq: 0 R: Extra0Reg.
	jump2 := self JumpNonZero: 0.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra0Reg.
	jump2 := self JumpLess: 0.
	self AndCq: 16rFFFF R: ClassReg.
	self CmpCq: 255 R: ClassReg.
	jump4 := self JumpGreaterOrEqual: 0.
	self CmpCq: 1 R: ClassReg.
	jump5 := self JumpLess: 0.
	self MoveCq: 8 R: SendNumArgsReg.
	self MulR: ClassReg R: SendNumArgsReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveCq: 8 R: SendNumArgsReg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self AddCq: 8 R: SendNumArgsReg.
	self MoveAw: objectMemory freeStartAddress R: Extra1Reg.
	self AddR: SendNumArgsReg R: Extra1Reg.
	self CmpCq: objectMemory getScavengeThreshold R: Extra1Reg.
	jump6 := self JumpGreater: 0.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: ClassReg R: Extra1Reg.
	self ArithmeticShiftRightCq: 56 R: Extra1Reg.
	self CmpCq: 0 R: Extra1Reg.
	jump4 := self JumpGreater: 0.
	self MoveCq: 8 R: Extra1Reg.
	self MulR: ClassReg R: Extra1Reg.
	self AddCq: 16 R: Extra1Reg.
	self MoveAw: objectMemory freeStartAddress R: Extra2Reg.
	self AddR: Extra1Reg R: Extra2Reg.
	self CmpCq: objectMemory getScavengeThreshold R: Extra2Reg.
	jump7 := self JumpGreater: 0.
	self MoveR: Extra1Reg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra1Reg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveAw: objectMemory freeStartAddress R: Extra2Reg.
	self CmpCq: 255 R: ClassReg.
	jump5 := self JumpGreaterOrEqual: 0.
	self MoveR: ClassReg R: Extra1Reg.
	self LogicalShiftLeftCq: 56 R: Extra1Reg.
	self LogicalShiftLeftCq: 24 R: TempReg.
	self AddR: TempReg R: Extra1Reg.
	self AddR: Extra0Reg R: Extra1Reg.
	self OrCq: 0 R: Extra1Reg.
	self MoveR: Extra1Reg M64: 0 r: Extra2Reg.
	self MoveR: Extra2Reg R: Extra1Reg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveR: ClassReg R: Extra1Reg.
	self AddCq: 16rFF00000000000000 R: Extra1Reg.
	self MoveR: Extra1Reg M64: 0 r: Extra2Reg.
	self MoveR: Extra2Reg R: Extra1Reg.
	self AddCq: 8 R: Extra1Reg.
	self LogicalShiftLeftCq: 24 R: TempReg.
	self AddCq: 16rFF00000000000000 R: TempReg.
	self AddR: Extra0Reg R: TempReg.
	self OrCq: 0 R: TempReg.
	self MoveR: TempReg M64: 0 r: Extra1Reg.
	self AddCq: 8 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra1Reg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveAw: objectMemory freeStartAddress R: Extra2Reg.
	self AddR: SendNumArgsReg R: Extra2Reg.
	self MoveR: Extra2Reg Aw: objectMemory freeStartAddress.
	self MoveR: Extra1Reg R: Extra2Reg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra1Reg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveCq: 0 R: Extra2Reg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra2Reg.
	jump8 := self JumpZero: 0.
	self MoveR: Extra2Reg R: Extra1Reg.
	self AddCq: 8 R: Extra1Reg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self AddCq: 8 R: SendNumArgsReg.
	self MoveCq: 8 R: Extra0Reg.
	self MulR: ClassReg R: Extra0Reg.
	self AddR: Extra0Reg R: SendNumArgsReg.
	self SubCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	b491 := self Label.
	self CmpR: Extra1Reg R: Extra0Reg.
	jump4 := self JumpGreaterOrEqual: 0.
	self MoveR: SendNumArgsReg R: ClassReg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveCq: objectMemory nilObject R: ClassReg.
	self MoveR: ClassReg M64: 0 r: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self AddCq: 8 R: ClassReg.
	self MoveR: ClassReg R: Extra1Reg.
	jump4 := self Jump: b491.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: ClassReg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self MoveR: ClassReg R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self CmpCq: 0 R: SendNumArgsReg.
	jump1 := self JumpZero: 0.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNewWithArg [

	| jump1 jump10 jumpNext jump13 jump9 jump6 jump3 jump12 currentBlock jump8 jump5 jump2 jumpTrue jump11 b1388 jump7 jump4 |
	self AddCq: 32 R: SPReg.
	self TstCq: 1 R: Arg0Reg.
	jump1 := self JumpZero: 0.
	self MoveR: Arg0Reg R: TempReg.
	self ArithmeticShiftRightCq: 3 R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump2 := self JumpLess: 0.
	self MoveCq: 8 R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self AddCq: 16 R: ClassReg.
	self MoveM64: 0 r: ClassReg R: SendNumArgsReg.
	self ArithmeticShiftRightCq: 3 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ClassReg.
	self ArithmeticShiftRightCq: 16 R: ClassReg.
	self AndCq: 31 R: ClassReg.
	self MoveCq: 4 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self AddR: Extra1Reg R: Extra0Reg.
	self MoveM32: 0 r: Extra0Reg R: Extra1Reg.
	self AndCq: 16rFFFFFFFF R: Extra1Reg.
	self AndCq: 16r3FFFFF R: Extra1Reg.
	self MoveCq: 0 R: Extra0Reg.
	self CmpCq: 2 R: ClassReg.
	jump3 := self JumpZero: 0.
	self CmpCq: 3 R: ClassReg.
	jump4 := self JumpZero: 0.
	self CmpCq: 4 R: ClassReg.
	jump5 := self JumpZero: 0.
	self CmpCq: 9 R: ClassReg.
	jump6 := self JumpZero: 0.
	self CmpCq: 10 R: ClassReg.
	jump7 := self JumpZero: 0.
	self CmpCq: 12 R: ClassReg.
	jump8 := self JumpZero: 0.
	self CmpCq: 16 R: ClassReg.
	jump9 := self JumpZero: 0.
	self CmpCq: 0 R: TempReg.
	jumpTrue := self JumpNonZero: 0.
	self MoveCq: 0 R: Extra0Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra0Reg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 0 R: TempReg.
	jump10 := self JumpNonZero: 0.
	self CmpCq: 5 R: ClassReg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: Extra0Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra0Reg.
	jumpNext jmpTarget: self Label.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra0Reg.
	jump11 := self JumpNonZero: 0.
	self AndCq: 16rFFFF R: SendNumArgsReg.
	self MoveCq: objectMemory nilObject R: Extra0Reg.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self MoveR: TempReg R: SendNumArgsReg.
	self AddCq: 7 R: SendNumArgsReg.
	self MoveCq: 8 R: Extra2Reg.
	self
		DivR: Extra2Reg
		R: SendNumArgsReg
		Quo: Extra2Reg
		Rem: SendNumArgsReg.
	self MoveCq: 8 R: SendNumArgsReg.
	self SubR: TempReg R: SendNumArgsReg.
	self AndCq: 7 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	jump9 := self Jump: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveR: TempReg R: SendNumArgsReg.
	self AddCq: 3 R: SendNumArgsReg.
	self MoveCq: 4 R: Extra2Reg.
	self
		DivR: Extra2Reg
		R: SendNumArgsReg
		Quo: Extra2Reg
		Rem: SendNumArgsReg.
	self MoveCq: 4 R: SendNumArgsReg.
	self SubR: TempReg R: SendNumArgsReg.
	self AndCq: 3 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self CmpCq: 34 R: Extra1Reg.
	jump7 := self JumpZero: 0.
	self MoveR: TempReg R: SendNumArgsReg.
	self AddCq: 1 R: SendNumArgsReg.
	self MoveCq: 2 R: Extra2Reg.
	self
		DivR: Extra2Reg
		R: SendNumArgsReg
		Quo: Extra2Reg
		Rem: SendNumArgsReg.
	self AndCq: 1 R: TempReg.
	self AddR: TempReg R: ClassReg.
	self MoveCq: 0 R: SendNumArgsReg.
	self CmpCq: 0 R: Extra1Reg.
	jump12 := self JumpZero: 0.
	jump13 := self Jump: 0.
	currentBlock := self Label.
	jump12 jmpTarget: currentBlock.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self CmpCq: 2 R: TempReg.
	jumpTrue := self JumpNonZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 0 R: SendNumArgsReg.
	jump7 := self JumpNonZero: 0.
	self MoveR: TempReg R: SendNumArgsReg.
	self AddCq: 1 R: SendNumArgsReg.
	self MoveCq: 2 R: Extra2Reg.
	self
		DivR: Extra2Reg
		R: SendNumArgsReg
		Quo: SendNumArgsReg
		Rem: Extra2Reg.
	self AndCq: 1 R: TempReg.
	self AddR: TempReg R: ClassReg.
	self MoveCq: 0 R: TempReg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self MoveR: TempReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: TempReg.
	jump13 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveR: TempReg R: SendNumArgsReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self AndCq: 16rFFFF R: SendNumArgsReg.
	self AddR: TempReg R: SendNumArgsReg.
	self MoveCq: objectMemory nilObject R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self AndCq: 16rFFFF R: SendNumArgsReg.
	self AddR: TempReg R: SendNumArgsReg.
	self MoveCq: objectMemory nilObject R: TempReg.
	self MoveR: TempReg R: Extra0Reg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: objectMemory nilObject R: Extra2Reg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra0Reg.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	jump9 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra1Reg.
	jump4 := self JumpZero: 0.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	jump13 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	currentBlock := self Label.
	jump12 jmpTarget: currentBlock.
	self MoveCq: 4 R: Extra1Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	self AddR: TempReg R: Extra1Reg.
	self MoveM32: 0 r: Extra1Reg R: TempReg.
	self AndCq: 16rFFFFFFFF R: TempReg.
	self AndCq: 16r3FFFFF R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump12 := self JumpZero: 0.
	self CmpCq: 0 R: TempReg.
	jump4 := self JumpLess: 0.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: TempReg R: Extra1Reg.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self CmpCq: 255 R: Extra3Reg.
	jump13 := self JumpGreaterOrEqual: 0.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self CmpCq: 1 R: Extra3Reg.
	jump5 := self JumpLess: 0.
	self MoveCq: 8 R: Extra2Reg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MulR: Extra3Reg R: Extra2Reg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveCq: 8 R: Extra2Reg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self AddCq: 8 R: Extra2Reg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self ArithmeticShiftRightCq: 56 R: Extra2Reg.
	self CmpCq: 0 R: Extra2Reg.
	jump13 := self JumpGreater: 0.
	self MoveCq: 8 R: Extra2Reg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MulR: Extra3Reg R: Extra2Reg.
	self AddCq: 16 R: Extra2Reg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveAw: objectMemory freeStartAddress R: ClassReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self CmpCq: 255 R: Extra3Reg.
	jump6 := self JumpGreaterOrEqual: 0.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self LogicalShiftLeftCq: 56 R: SendNumArgsReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: SendNumArgsReg.
	self AddR: Extra1Reg R: SendNumArgsReg.
	self OrCq: 0 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg M64: 0 r: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self AddCq: 16rFF00000000000000 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg M64: 0 r: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self AddCq: 8 R: SendNumArgsReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self AddCq: 16rFF00000000000000 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self AddR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self OrCq: 0 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg M64: 0 r: SendNumArgsReg.
	self AddCq: 8 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveAw: objectMemory freeStartAddress R: ClassReg.
	self AddR: Extra2Reg R: ClassReg.
	self MoveR: ClassReg Aw: objectMemory freeStartAddress.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveCq: 0 R: ClassReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump5 := self JumpZero: 0.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self AddCq: 8 R: SendNumArgsReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self AddCq: 8 R: Extra2Reg.
	self MoveCq: 8 R: Extra1Reg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MulR: Extra3Reg R: Extra1Reg.
	self AddR: Extra1Reg R: Extra2Reg.
	self SubCq: 1 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra1Reg.
	b1388 := self Label.
	self CmpR: SendNumArgsReg R: Extra1Reg.
	jump13 := self JumpGreaterOrEqual: 0.
	self MoveR: TempReg R: Extra2Reg.
	self MoveR: Extra0Reg R: ClassReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg M64: 0 r: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ClassReg.
	self AddCq: 8 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	jump13 := self Jump: b1388.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra2Reg.
	self MoveR: Extra0Reg R: ClassReg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveR: Extra2Reg R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra0Reg.
	jump11 := self JumpZero: 0.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self SubCq: 32 R: SPReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self MoveR: ClassReg R: Arg0Reg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	jump7 jmpTarget: currentBlock.
	jump12 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	self SubCq: 32 R: SPReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNextUint16At [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	self MoveM16: 0 r: TempReg R: ClassReg.
	self AndCq: 16rFFFF R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNoopLoopWithInvariant [

	| jump1 jump2 b20 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpLess: 0.
	self MoveCq: 3 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: TempReg.
	b20 := self Label.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self CmpCq: 10 R: ClassReg.
	jump1 := self JumpLess: 0.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ClassReg R: TempReg.
	jump1 := self Jump: b20.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNotEqualsThan [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNotEqualsThanFloats [

	| jumpNext currentBlock jumpTrue |
	self MoveR: Arg0Reg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg0.
	self MoveR: ReceiverResultReg R: TempReg.
	self ConvertR: TempReg Rd: DPFPReg1.
	self CmpRd: DPFPReg1 Rd: DPFPReg0.
	jumpTrue := self JumpFPNotEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNotEqualsThanInverted [

	| jump1 jump2 currentBlock |
	self MoveCq: 10 R: TempReg.
	self CmpR: ReceiverResultReg R: TempReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveOr [

	| jumpNext jumpTrue jump1 currentBlock jump2 |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 5 R: TempReg.
	jumpTrue := self JumpLess: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: ReceiverResultReg R: ClassReg.
	self CmpCq: 5 R: ClassReg.
	jump1 := self JumpLess: 0.
	self MoveR: ReceiverResultReg R: ClassReg.
	self CmpCq: 10 R: ClassReg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: ClassReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: ClassReg.
	jumpNext jmpTarget: self Label.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: TempReg R: ClassReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self CmpCq: 0 R: ClassReg.
	jump2 := self JumpNonZero: 0.
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
DruidTestRTLCompiler >> gen_primitiveReturnFalse [

	| currentBlock |
	self MoveCq: objectMemory falseObject R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOne [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnOneWithArguments [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveReturnTrue [

	| currentBlock |
	self MoveCq: objectMemory trueObject R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSandclock [

	| jump1 jump2 jump3 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 99 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self CmpCq: 0 R: ClassReg.
	jump2 := self JumpGreater: 0.
	self MoveCq: 116 R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveR: TempReg R: ClassReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 1 R: ClassReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self AddCq: 5 R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSizeofusqInt [

	| currentBlock |
	self MoveCq: 4 R: ReceiverResultReg.
	self genPrimReturn.
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

	| jump1 jump2 currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self SubR: TempReg R: ClassReg.
	jump1 := self JumpOverflow: 0.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 99 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSumWithOverflow [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	jump1 := self JumpOverflow: 0.
	self MoveR: TempReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 99 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveTrueAssignmentAsInteger [

	| currentBlock |
	self MoveCq: 1 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveTrueAssignmentAsIntegerShifted [

	| currentBlock |
	self MoveCq: 2 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveTrueNot [

	| currentBlock |
	self MoveCq: 0 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint16At [

	| currentBlock |
	self MoveM16: 0 r: ReceiverResultReg R: TempReg.
	self AndCq: 16rFFFF R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint16AtPut [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: TempReg M16: 0 r: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint32At [

	| currentBlock |
	self MoveM32: 0 r: ReceiverResultReg R: TempReg.
	self AndCq: 16rFFFFFFFF R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint32AtPut [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: TempReg M32: 0 r: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint64At [

	| currentBlock |
	self MoveM64: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint64AtPut [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: TempReg M64: 0 r: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint8At [

	| currentBlock |
	self MoveMb: 0 r: ReceiverResultReg R: TempReg.
	self AndCq: 255 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint8AtPut [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: TempReg Mb: 0 r: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 55 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfAssigningValue [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 57 R: ReceiverResultReg.
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
DruidTestRTLCompiler >> gen_primitiveWithIfNilIfNotNilStatement [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfNotNilIfNilStatement [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfNotNilIfNilStatementWithArgument [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfNotNilStatement [

	| jump1 jump2 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfReturningValue [

	| currentBlock jump0 jump1 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 57 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithNonCompilableBranch [

	| jump1 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithSequentialExitPoint [

	| currentBlock |
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_pushConstantTrueBytecode [

	| currentBlock |
	self ssPushConstant: objectMemory trueObject.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_pushLiteralConstantBytecode [

	| currentBlock |
	self MoveR: FPReg R: TempReg.
	self MoveM64: FoxMethod r: TempReg R: TempReg.
	self AddCq: 8 R: TempReg.
	self MoveCq: LiteralStart R: ClassReg.
	self LogicalShiftLeftCq: 2 R: ClassReg.
	self AddR: ClassReg R: TempReg.
	self ssPushBase: TempReg offset: 0.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_pushReceiverBytecode [

	| currentBlock |
	self ensureReceiverResultRegContainsSelf.
	self MoveR: ReceiverResultReg R: TempReg.
	self ssPushRegister: TempReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_pushReceiverVariableBytecode [

	| currentBlock |
	self ensureReceiverResultRegContainsSelf.
	self MoveR: ReceiverResultReg R: TempReg.
	self ssPushBase: TempReg offset: 16.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_sendLiteralSelector0ArgsBytecode [

	| currentBlock |
	self marshallSendArguments: 0.
	self
		genMarshalledSend: 1
		numArgs: 0
		sendTable: ordinarySendTrampolines.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_shortUnconditionalJump [

	| currentBlock |
	self MoveCq: 1 R: TempReg.
	self AddCq: bytecodePC R: TempReg.
	self AddCq: 1 R: TempReg.
	self genJumpTo: 1 + bytecodePC + 1.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> numRegArgs [

	^ 2 "Hardcoded?"
]

{ #category : #compatibility }
DruidTestRTLCompiler >> shouldAnnotateObjectReference: anOop [
	"Objects in newSpace or oldSpace except nil, true, false & classTableRootObj need to be annotated."
	^(objectMemory isNonImmediate: anOop)
	  and: [(self oop: anOop isGreaterThan: objectMemory classTableRootObj)
		or: [self oop: anOop isLessThan: objectMemory nilObject]]
]

{ #category : #generated }
DruidTestRTLCompiler >> smallIntegerSpecialisedPreamble [ 

	self mclassIsSmallInteger ifFalse:
		[^UnimplementedPrimitive].
]
