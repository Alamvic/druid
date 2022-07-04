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

	| jump0 currentBlock |
	self CmpCq: 0 R: Arg0Reg.
	jump0 := self JumpNonZero: 0.
	self genPrimReturn.
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
DruidTestRTLCompiler >> gen_ifZeroFailPrimitive [

	| jump0 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
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

	| jump0 jump1 jump2 currentBlock |
	self mclassIsSmallInteger ifFalse: [ ^ UnimplementedPrimitive ].
	self MoveCq: 1 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self AndR: ClassReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: TempReg.
	self AndR: TempReg R: ClassReg.
	self CmpCq: 0 R: ClassReg.
	jump1 := self JumpZero: 0.
	self MoveR: Arg0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	jump2 := self JumpOverflow: 0.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	jump1 jmpTarget: currentBlock.
	jump2 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAnd [

	| jumpNext jumpTrue jump1 jump3 currentBlock jump4 jump2 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 0 R: TempReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self CmpCq: 0 R: TempReg.
	jump3 := self JumpNonZero: 0.
	self MoveCq: 10 R: ReceiverResultReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self genPrimReturn.
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

	| jump1 jump10 jumpNext jump9 jump6 jump3 currentBlock jump8 jump5 jumpTrue jump2 jump11 jump7 jump4 |
	self AddCq: 512 R: SPReg.
	self MoveR: ReceiverResultReg R: TempReg.
	self TstCq: 7 R: ReceiverResultReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 1 R: ClassReg.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self AndR: SendNumArgsReg R: ClassReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self CmpCq: 0 R: ClassReg.
	jump2 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self MoveR: Arg0Reg R: Extra0Reg.
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self AndCq: 1 R: ClassReg.
	self CmpCq: 0 R: ClassReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: Extra2Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra2Reg.
	jumpNext jmpTarget: self Label.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	self CmpCq: 0 R: ClassReg.
	jump2 := self JumpZero: 0.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra2Reg.
	jump2 := self JumpNonZero: 0.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self LogicalShiftRightCq: 3 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveM64: 0 r: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 72 r: SPReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self LogicalShiftRightCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self AndCq: 31 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self AddCq: 7 R: SendNumArgsReg.
	self MoveMw: 0 r: SendNumArgsReg R: SendNumArgsReg.
	self AndCq: 255 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self CmpCq: 255 R: SendNumArgsReg.
	jump3 := self JumpNonZero: 0.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self SubCq: 8 R: TempReg.
	self MoveM64: 0 r: TempReg R: TempReg.
	self LogicalShiftLeftCq: 8 R: TempReg.
	self LogicalShiftRightCq: 8 R: TempReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 5 R: Extra3Reg.
	jump3 := self JumpLessOrEqual: 0.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 16 R: Extra3Reg.
	jump4 := self JumpGreaterOrEqual: 0.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 12 R: Extra3Reg.
	jump5 := self JumpGreaterOrEqual: 0.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 10 R: Extra3Reg.
	jump6 := self JumpGreaterOrEqual: 0.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	jump7 := self JumpZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 1 R: TempReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self AndCq: 1 R: SendNumArgsReg.
	self SubR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 2 R: TempReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self AndCq: 3 R: SendNumArgsReg.
	self SubR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 3 R: TempReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self AndCq: 7 R: SendNumArgsReg.
	self SubR: SendNumArgsReg R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	jump7 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	jump5 := self JumpGreaterOrEqual: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self CmpCq: 9 R: TempReg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 96 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	jump5 := self JumpGreaterOrEqual: 0.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self CmpCq: 2 R: TempReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 96 r: SPReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 104 r: SPReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 112 r: SPReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 120 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 160 r: SPReg.
	self MoveR: Extra3Reg Mw: 128 r: SPReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 136 r: SPReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 144 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 152 r: SPReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveMw: 96 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump5 := self JumpNonZero: 0.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 168 r: SPReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 176 r: SPReg.
	self MoveMw: 144 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 232 r: SPReg.
	self MoveMw: 136 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 240 r: SPReg.
	self MoveMw: 128 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 256 r: SPReg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 264 r: SPReg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 272 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self CmpCq: 2 R: Extra3Reg.
	jump6 := self JumpLess: 0.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveM64: 0 r: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 184 r: SPReg.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self AndCq: 16r3FFFFF R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 184 r: SPReg.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self CmpCq: 31 R: Extra3Reg.
	jump8 := self JumpGreater: 0.
	self MoveMw: 160 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 192 r: SPReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 200 r: SPReg.
	self MoveMw: 144 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 208 r: SPReg.
	self MoveMw: 136 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 128 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self CmpCq: 31 R: Extra3Reg.
	jump7 := self JumpZero: 0.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self CmpCq: 8 R: Extra3Reg.
	jump3 := self JumpZero: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self LogicalShiftRightCq: 10 R: Extra0Reg.
	self MoveCq: 8 R: Extra2Reg.
	self AddCq: objectMemory nilObject R: Extra2Reg.
	self LogicalShiftLeftCq: 3 R: Extra0Reg.
	self AddR: Extra0Reg R: Extra2Reg.
	self MoveM64: 0 r: Extra2Reg R: Extra2Reg.
	self CmpCq: objectMemory nilObject R: Extra2Reg.
	jump8 := self JumpZero: 0.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self AndCq: 1023 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 184 r: SPReg.
	self AddCq: 8 R: Extra2Reg.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 3 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 184 r: SPReg.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: Extra2Reg.
	self MoveM64: 0 r: Extra2Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 216 r: SPReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveCq: objectMemory nilObject R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 216 r: SPReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 224 r: SPReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 216 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 192 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 200 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 208 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self MoveMw: 224 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveCq: objectMemory nilObject R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 192 r: SPReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 200 r: SPReg.
	self MoveMw: 144 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 208 r: SPReg.
	self MoveMw: 136 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 128 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	self MoveMw: 192 r: SPReg R: Extra3Reg.
	self AddCq: 8 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 192 r: SPReg.
	self MoveMw: 192 r: SPReg R: Extra3Reg.
	self AddCq: 16 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 192 r: SPReg.
	self MoveMw: 192 r: SPReg R: Extra3Reg.
	self MoveM64: 0 r: Extra3Reg R: SendNumArgsReg.
	self LogicalShiftRightCq: 3 R: SendNumArgsReg.
	self AndCq: 16rFFFF R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 168 r: SPReg.
	self MoveMw: 200 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 176 r: SPReg.
	self MoveMw: 208 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 232 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 240 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 256 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 264 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 272 r: SPReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 168 r: SPReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 176 r: SPReg.
	self MoveMw: 144 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 232 r: SPReg.
	self MoveMw: 136 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 240 r: SPReg.
	self MoveMw: 128 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 256 r: SPReg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 264 r: SPReg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 272 r: SPReg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	self MoveMw: 176 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 280 r: SPReg.
	self MoveMw: 232 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 288 r: SPReg.
	self MoveMw: 240 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 368 r: SPReg.
	self MoveMw: 248 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 312 r: SPReg.
	self MoveMw: 256 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 320 r: SPReg.
	self MoveMw: 168 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 328 r: SPReg.
	self MoveMw: 264 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 336 r: SPReg.
	self MoveMw: 272 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 344 r: SPReg.
	self MoveMw: 176 r: SPReg R: Extra3Reg.
	self CmpCq: 3 R: Extra3Reg.
	jump4 := self JumpNonZero: 0.
	self MoveMw: 272 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 296 r: SPReg.
	self MoveMw: 264 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 304 r: SPReg.
	self MoveMw: 168 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 256 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 248 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 240 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 232 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 176 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 280 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 288 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 368 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 312 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 320 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 328 r: SPReg.
	self MoveMw: 304 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 336 r: SPReg.
	self MoveMw: 296 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 344 r: SPReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveMw: 280 r: SPReg R: Extra3Reg.
	self CmpCq: 3 R: Extra3Reg.
	jump4 := self JumpZero: 0.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 352 r: SPReg.
	self MoveMw: 280 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 360 r: SPReg.
	self MoveMw: 288 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 384 r: SPReg.
	self MoveMw: 312 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 392 r: SPReg.
	self MoveMw: 320 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 400 r: SPReg.
	self MoveMw: 328 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 408 r: SPReg.
	self MoveMw: 336 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 416 r: SPReg.
	self MoveMw: 344 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 424 r: SPReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveMw: 344 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 376 r: SPReg.
	self MoveMw: 336 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 328 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 320 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 312 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 288 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 280 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 368 r: SPReg R: Extra3Reg.
	self AndCq: 16r3FFFFF R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 368 r: SPReg.
	self MoveMw: 368 r: SPReg R: Extra3Reg.
	self CmpCq: 36 R: Extra3Reg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: Extra3Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra3Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra3Reg Mw: 368 r: SPReg.
	self MoveMw: 368 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 352 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 360 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 384 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 392 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 400 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 408 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 416 r: SPReg.
	self MoveMw: 376 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 424 r: SPReg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveMw: 352 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump6 := self JumpNonZero: 0.
	self MoveMw: 384 r: SPReg R: Extra3Reg.
	self MoveMw: 408 r: SPReg R: Extra4Reg.
	self SubR: Extra4Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 384 r: SPReg.
	self MoveMw: 360 r: SPReg R: Extra3Reg.
	self CmpCq: 24 R: Extra3Reg.
	jump4 := self JumpGreaterOrEqual: 0.
	self MoveMw: 424 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 432 r: SPReg.
	self MoveMw: 416 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 408 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 400 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 384 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 360 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 392 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 440 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 456 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 464 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 472 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 480 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 488 r: SPReg.
	self MoveMw: 432 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 496 r: SPReg.
	self MoveMw: 400 r: SPReg R: Extra3Reg.
	self CmpCq: 1 R: Extra3Reg.
	jump5 := self JumpLess: 0.
	self MoveMw: 424 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 448 r: SPReg.
	self MoveMw: 416 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 408 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 400 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 384 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 360 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 392 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 440 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 456 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 464 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 472 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 480 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 488 r: SPReg.
	self MoveMw: 448 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 496 r: SPReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveMw: 456 r: SPReg R: Extra3Reg.
	self CmpCq: 24 R: Extra3Reg.
	jump5 := self JumpGreaterOrEqual: 0.
	self MoveMw: 496 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 504 r: SPReg.
	self MoveMw: 488 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 480 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 472 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 456 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 440 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 472 r: SPReg R: Extra3Reg.
	self CmpCq: 1 R: Extra3Reg.
	jump7 := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 512 r: SPReg.
	self MoveMw: 504 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveMw: 496 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 488 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 480 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 472 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 456 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 440 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 472 r: SPReg R: Extra3Reg.
	self MoveMw: 464 r: SPReg R: Extra4Reg.
	self CmpR: Extra4Reg R: Extra3Reg.
	jumpTrue := self JumpLessOrEqual: 0.
	self MoveCq: 0 R: Extra3Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra3Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra3Reg Mw: 472 r: SPReg.
	self MoveMw: 472 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 512 r: SPReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveMw: 512 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump3 := self JumpNonZero: 0.
	self CmpCq: 1 R: ClassReg.
	jump7 := self JumpLessOrEqual: 0.
	self MoveCq: 4 R: Extra1Reg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveCq: 2 R: Extra1Reg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveCq: 0 R: ClassReg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self AddR: TempReg R: SendNumArgsReg.
	self CmpCq: 5 R: ClassReg.
	jump3 := self JumpLessOrEqual: 0.
	self CmpCq: 16 R: ClassReg.
	jump7 := self JumpGreaterOrEqual: 0.
	self CmpCq: 12 R: ClassReg.
	jump9 := self JumpGreaterOrEqual: 0.
	self CmpCq: 9 R: ClassReg.
	jump10 := self JumpZero: 0.
	self MoveCq: 0 R: Extra1Reg.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Arg0Reg.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self SubCq: 1 R: SendNumArgsReg.
	self AddCq: 8 R: Extra1Reg.
	self LogicalShiftLeftCq: 1 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: Extra1Reg.
	self MoveM16: 0 r: Extra1Reg R: Extra1Reg.
	self AndCq: 16rFFFF R: Extra1Reg.
	self LogicalShiftLeftCq: 3 R: Extra1Reg.
	self AddCq: 1 R: Extra1Reg.
	jump9 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self SubCq: 1 R: SendNumArgsReg.
	self AddCq: 8 R: Extra1Reg.
	self AddR: SendNumArgsReg R: Extra1Reg.
	self MoveMw: 0 r: Extra1Reg R: Extra1Reg.
	self AndCq: 255 R: Extra1Reg.
	self LogicalShiftLeftCq: 3 R: Extra1Reg.
	self AddCq: 1 R: Extra1Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self SubCq: 1 R: SendNumArgsReg.
	self AddCq: 8 R: Extra1Reg.
	self LogicalShiftLeftCq: 3 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: Extra1Reg.
	self MoveM64: 0 r: Extra1Reg R: Extra1Reg.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	jump9 jmpTarget: currentBlock.
	jump7 jmpTarget: currentBlock.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveR: Extra1Reg R: ClassReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra1Reg.
	jump8 := self JumpZero: 0.
	self MoveR: Extra0Reg R: Arg0Reg.
	self MoveR: Extra2Reg R: ReceiverResultReg.
	self CmpCq: 0 R: Extra1Reg.
	jump7 := self JumpZero: 0.
	jump9 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveR: ClassReg R: ReceiverResultReg.
	self CmpCq: 0 R: Extra1Reg.
	jump8 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self SubCq: 512 R: SPReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveMw: 488 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveMw: 496 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveMw: 416 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveMw: 424 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveMw: 416 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveMw: 424 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump10 jmpTarget: currentBlock.
	jump9 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	self SubCq: 512 R: SPReg.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveBitShift [

	| jump1 jump2 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump1 := self JumpGreater: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self NegateR: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self LogicalShiftRightR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: TempReg.
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
	self CmpCq: 0 R: ReceiverResultReg.
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

	| jump0 jump1 currentBlock |
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

	| jump3 jump1 currentBlock jump4 jump2 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump1 := self JumpZero: 0.
	self CmpCq: 1 R: ReceiverResultReg.
	jump2 := self JumpZero: 0.
	self MoveCq: 77 R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 57 R: ReceiverResultReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
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

	| jump0 jump1 currentBlock |
	self CmpCq: objectMemory trueObject R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 2 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveDNA [

	| jump3 jump1 currentBlock jump4 jump2 jump0 |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self CmpCq: 0 R: ReceiverResultReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 17 R: TempReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 99 R: TempReg.
	self CmpCq: 7 R: ReceiverResultReg.
	jump3 := self JumpGreater: 0.
	self MoveCq: 5 R: TempReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self AddCq: 99 R: TempReg.
	self AddCq: 42 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump4 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: TempReg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self AddCq: 5 R: TempReg.
	self AddCq: 15 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
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
	self
		DivR: Arg0Reg
		R: ReceiverResultReg
		Quo: ClassReg
		Rem: TempReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThan [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveEqualsThanInverted [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 0 R: TempReg.
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
DruidTestRTLCompiler >> gen_primitiveFalseAsInteger [

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

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThanInverted [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThan [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterThanInverted [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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

	| jump0 jump1 currentBlock |
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
DruidTestRTLCompiler >> gen_primitiveIfTrueIfFalseAssigningValue [

	| jump0 jump1 currentBlock |
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
DruidTestRTLCompiler >> gen_primitiveIfTrueIfFalseReturningValue [

	| jump0 jump1 currentBlock |
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
DruidTestRTLCompiler >> gen_primitiveIfTrueStatement [

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 10 R: ReceiverResultReg.
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
DruidTestRTLCompiler >> gen_primitiveImplicitArgumentBitShiftLeft [

	| jump1 jump2 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump1 := self JumpGreater: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self NegateR: TempReg.
	self MoveCq: 1 R: ClassReg.
	self LogicalShiftRightR: TempReg R: ClassReg.
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
	self LogicalShiftRightCq: 1 R: TempReg.
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
DruidTestRTLCompiler >> gen_primitiveIsIntegerObject [

	| jump0 jump1 currentBlock |
	self MoveCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AndR: ClassReg R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump0 := self JumpNonZero: 0.
	self MoveCw: objectMemory falseObject R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCw: objectMemory trueObject R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThanInverted [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreaterOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThan [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLessThanInverted [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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

	| jump0 currentBlock b14 |
	self MoveCq: 10 R: TempReg.
	b14 := self Label.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self CmpCq: 15 R: ClassReg.
	jump0 := self JumpLess: b14.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLoopIncrementingGlobalState [

	| jump0 currentBlock b14 |
	self MoveCq: 10 R: TempReg.
	b14 := self Label.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self CmpCq: 15 R: ClassReg.
	jump0 := self JumpLess: b14.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
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
	self LogicalShiftRightCq: 3 R: TempReg.
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

	| jump1 jumpNext jump6 jump3 currentBlock jump8 jump5 jumpTrue jump2 b481 jump7 jump4 |
	self AddCq: 48 R: SPReg.
	self MoveCq: 8 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	self AddCq: 16 R: TempReg.
	self MoveM64: 0 r: TempReg R: ClassReg.
	self LogicalShiftRightCq: 3 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self LogicalShiftRightCq: 16 R: TempReg.
	self AndCq: 31 R: TempReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self CmpCq: 1 R: SendNumArgsReg.
	jumpTrue := self JumpLessOrEqual: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 1 R: TempReg.
	jump1 := self JumpLessOrEqual: 0.
	self MoveR: TempReg R: SendNumArgsReg.
	self CmpCq: 5 R: SendNumArgsReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	jumpNext jmpTarget: self Label.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self CmpCq: 0 R: SendNumArgsReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 4 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: Extra1Reg.
	self AddR: Extra1Reg R: Extra0Reg.
	self MoveM32: 0 r: Extra0Reg R: Extra1Reg.
	self AndCq: 16rFFFFFFFF R: Extra1Reg.
	self AndCq: 16r3FFFFF R: Extra1Reg.
	self CmpCq: 0 R: Extra1Reg.
	jump1 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self CmpCq: 0 R: Extra1Reg.
	jump3 := self JumpLess: 0.
	self AndCq: 16rFFFF R: ClassReg.
	self CmpCq: 255 R: ClassReg.
	jump4 := self JumpGreaterOrEqual: 0.
	self CmpCq: 1 R: ClassReg.
	jump5 := self JumpLess: 0.
	self MoveCq: 8 R: Extra2Reg.
	self MulR: ClassReg R: Extra2Reg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveCq: 8 R: Extra2Reg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: ClassReg R: Extra0Reg.
	self AddCq: 8 R: Extra2Reg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveR: Extra0Reg R: ClassReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: ClassReg R: TempReg.
	self LogicalShiftRightCq: 56 R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump4 := self JumpGreater: 0.
	self MoveCq: 8 R: TempReg.
	self MulR: ClassReg R: TempReg.
	self AddCq: 16 R: TempReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveAw: objectMemory freeStartAddress R: Extra1Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self CmpCq: objectMemory getScavengeThreshold R: Extra2Reg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: Extra2Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra2Reg.
	jumpNext jmpTarget: self Label.
	self CmpCq: objectMemory getScavengeThreshold R: Extra1Reg.
	jump6 := self JumpGreater: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveCq: 0 R: TempReg.
	self CmpCq: objectMemory getScavengeThreshold R: Extra1Reg.
	jump6 := self JumpGreater: 0.
	self MoveR: TempReg R: Extra2Reg.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra2Reg.
	jump6 := self JumpNonZero: 0.
	self MoveAw: objectMemory freeStartAddress R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self CmpCq: 255 R: SendNumArgsReg.
	jump5 := self JumpGreaterOrEqual: 0.
	self LogicalShiftLeftCq: 56 R: SendNumArgsReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: SendNumArgsReg.
	self OrCq: 0 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg M64: 0 r: Extra2Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self AddCq: 16rFF00000000000000 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg M64: 0 r: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	self AddCq: 8 R: TempReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self AddCq: 16rFF00000000000000 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveMw: 16 r: SPReg R: Extra4Reg.
	self AddR: Extra4Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self OrCq: 0 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg M64: 0 r: TempReg.
	self AddCq: 8 R: Extra2Reg.
	self MoveR: Extra2Reg R: TempReg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveAw: objectMemory freeStartAddress R: Extra0Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: Extra0Reg.
	self MoveR: Extra0Reg Aw: objectMemory freeStartAddress.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self CmpCq: objectMemory getScavengeThreshold R: Extra1Reg.
	jump6 := self JumpGreater: 0.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	jump6 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: Extra0Reg R: Extra2Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveR: Extra2Reg R: Extra1Reg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump7 := self JumpZero: 0.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self AddCq: 8 R: Extra1Reg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self AddCq: 8 R: Extra0Reg.
	self MoveCq: 8 R: SendNumArgsReg.
	self MulR: ClassReg R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: Extra0Reg.
	self SubCq: 1 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveR: Extra1Reg R: TempReg.
	b481 := self Label.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpR: TempReg R: Extra3Reg.
	jump4 := self JumpGreaterOrEqual: 0.
	self MoveR: Extra2Reg R: Extra1Reg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra0Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveCq: 0 R: ClassReg.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra0Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveCq: objectMemory nilObject R: ClassReg.
	self MoveR: ClassReg M64: 0 r: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 8 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	jump4 := self Jump: b481.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ClassReg.
	self NegateR: ClassReg.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self MoveR: Extra0Reg R: Extra1Reg.
	self MoveR: ClassReg R: Extra0Reg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump7 jmpTarget: currentBlock.
	self CmpCq: 0 R: SendNumArgsReg.
	jump7 := self JumpZero: 0.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self CmpCq: 0 R: Extra0Reg.
	jump2 := self JumpZero: 0.
	jump3 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: Extra2Reg.
	self AddCq: 8 R: Extra2Reg.
	self AddCq: 16 R: Extra2Reg.
	self MoveM64: 0 r: Extra2Reg R: Extra2Reg.
	self LogicalShiftRightCq: 3 R: Extra2Reg.
	self LogicalShiftRightCq: 16 R: Extra2Reg.
	self AndCq: 31 R: Extra2Reg.
	self MoveR: Extra2Reg R: ClassReg.
	self CmpCq: 1 R: ClassReg.
	jumpTrue := self JumpLessOrEqual: 0.
	self MoveCq: 0 R: ClassReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: ClassReg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 1 R: Extra2Reg.
	jump7 := self JumpLessOrEqual: 0.
	self CmpCq: 5 R: Extra2Reg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: Extra2Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra2Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra2Reg R: ClassReg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self CmpCq: 0 R: ClassReg.
	jump7 := self JumpNonZero: 0.
	self MoveCq: 2 R: ClassReg.
	jump8 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveCq: 9 R: ClassReg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: ReceiverResultReg.
	self CmpCq: 0 R: ClassReg.
	jump8 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self SubCq: 48 R: SPReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	jump5 jmpTarget: currentBlock.
	jump6 jmpTarget: currentBlock.
	jump3 jmpTarget: currentBlock.
	jump8 jmpTarget: currentBlock.
	self SubCq: 48 R: SPReg.
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
DruidTestRTLCompiler >> gen_primitiveNotEqualsThan [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveNotEqualsThanInverted [

	| jump0 jump1 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 5 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveOr [

	| jumpNext jumpTrue jump1 currentBlock jump2 jump0 |
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 5 R: TempReg.
	jumpTrue := self JumpLess: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self CmpCq: 5 R: ReceiverResultReg.
	jump0 := self JumpLess: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self CmpCq: 10 R: TempReg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self CmpCq: 0 R: TempReg.
	jump1 := self JumpNonZero: 0.
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
DruidTestRTLCompiler >> gen_primitiveReturnFalse [

	| currentBlock |
	self MoveCw: objectMemory falseObject R: ReceiverResultReg.
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
	self MoveCw: objectMemory trueObject R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveSandclock [

	| jump0 jump1 jump2 currentBlock |
	self CmpCq: 10 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 99 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 116 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: TempReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self AddCq: 5 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
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

	| jump0 jump1 currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self SubR: TempReg R: ClassReg.
	jump0 := self JumpOverflow: 0.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 99 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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
	self MoveMw: 0 r: ReceiverResultReg R: TempReg.
	self AndCq: 255 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint8AtPut [

	| currentBlock |
	self MoveR: Arg0Reg R: TempReg.
	self MoveR: TempReg Mw: 0 r: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self MoveCq: 55 R: ReceiverResultReg.
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

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfNotNilIfNilStatement [

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfNotNilIfNilStatementWithArgument [

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithIfNotNilStatement [

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveCq: 42 R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 17 R: ReceiverResultReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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

	| jump0 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithSequentialExitPoint [

	| currentBlock |
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> numRegArgs [

	^ 2 "Hardcoded?"
]

{ #category : #generated }
DruidTestRTLCompiler >> smallIntegerSpecialisedPreamble [ 

	self mclassIsSmallInteger ifFalse:
		[^UnimplementedPrimitive].
]
