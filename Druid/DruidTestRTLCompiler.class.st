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

	| jump0 jump1 currentBlock |
	self mclassIsSmallInteger ifFalse: [ ^ UnimplementedPrimitive ].
	self TstCq: 1 R: Arg0Reg.
	jump0 := self JumpZero: 0.
	self MoveR: Arg0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	jump1 := self JumpOverflow: 0.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	jump1 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAnd [

	| jump0 jump1 jump2 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self CmpCq: 10 R: ReceiverResultReg.
	jump1 := self JumpGreater: 0.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
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
DruidTestRTLCompiler >> gen_primitiveAssertIsIgnored [

	| currentBlock |
	self MoveCq: 17 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveAt [

	| currentBlock jump0 jumpTrue jumpNext jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jumpTrue jumpNext jump9 jump10 jumpTrue jumpNext jump11 jumpTrue jumpNext jump12 jumpTrue jumpNext jump13 jumpTrue jumpNext jump14 jumpTrue jumpNext jump15 jump16 jump17 jump18 jump19 jump20 jump21 jump22 jump23 jump24 jump25 jumpTrue jumpNext jump26 jump27 jump28 jump29 jumpTrue jumpNext jump30 jump31 jump32 jump33 jump34 jump35 jump36 jump37 jump38 jump39 jump40 jump41 jump42 jump43 jump44 jump45 jump46 jump47 |
	self AddCq: 320 R: SPReg.
	self MoveR: ReceiverResultReg R: TempReg.
	self TstCq: 7 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
	self MoveR: Arg0Reg R: ClassReg.
	self AndCq: 1 R: ClassReg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self CmpCq: 0 R: SendNumArgsReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	jumpNext jmpTarget: self Label.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: ReceiverResultReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self CmpCq: 0 R: ClassReg.
	jump1 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Extra2Reg.
	self MoveR: Arg0Reg R: Extra1Reg.
	self MoveR: Arg0Reg R: Extra0Reg.
	self MoveR: ReceiverResultReg R: TempReg.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self CmpCq: 0 R: SendNumArgsReg.
	jump2 := self JumpNonZero: 0.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self LogicalShiftRightCq: 3 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveM64: 0 r: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self LogicalShiftRightCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self AndCq: 31 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self AddCq: 7 R: Extra1Reg.
	self MoveMw: 0 r: Extra1Reg R: Extra1Reg.
	self AndCq: 255 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra0Reg.
	self CmpCq: 255 R: Extra1Reg.
	jump3 := self JumpNonZero: 0.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self SubCq: 8 R: Extra0Reg.
	self MoveM64: 0 r: Extra0Reg R: Extra0Reg.
	self LogicalShiftLeftCq: 8 R: Extra0Reg.
	self LogicalShiftRightCq: 8 R: Extra0Reg.
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpCq: 5 R: Extra3Reg.
	jump4 := self JumpLessOrEqual: 0.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpCq: 16 R: Extra3Reg.
	jump5 := self JumpGreaterOrEqual: 0.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpCq: 12 R: Extra3Reg.
	jump6 := self JumpGreaterOrEqual: 0.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpCq: 10 R: Extra3Reg.
	jump7 := self JumpGreaterOrEqual: 0.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	jump8 := self JumpZero: 0.
	self MoveCq: 0 R: Extra0Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self CmpCq: 9 R: Extra1Reg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra1Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra1Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra0Reg R: TempReg.
	self MoveR: Extra1Reg R: Extra2Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	jump9 := self JumpGreaterOrEqual: 0.
	self MoveR: Extra1Reg R: Extra2Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self CmpCq: 9 R: Extra0Reg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra0Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra0Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: Extra0Reg R: Extra2Reg.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra0Reg.
	jump11 := self Jump: 0.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 1 R: Extra0Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self AndCq: 1 R: Extra2Reg.
	self SubR: Extra2Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self CmpCq: 9 R: TempReg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra2Reg R: Extra0Reg.
	self MoveR: TempReg R: Extra2Reg.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: TempReg.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 2 R: Extra0Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self AndCq: 3 R: Extra2Reg.
	self SubR: Extra2Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self CmpCq: 9 R: Extra0Reg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra0Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra0Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: Extra0Reg R: Extra2Reg.
	currentBlock := self Label.
	jump12 jmpTarget: currentBlock.
	self MoveR: TempReg R: Extra0Reg.
	jump13 := self Jump: 0.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self LogicalShiftLeftCq: 3 R: Extra0Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self AndCq: 7 R: Extra2Reg.
	self SubR: Extra2Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra2Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self CmpCq: 9 R: TempReg.
	jumpTrue := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra2Reg R: Extra0Reg.
	self MoveR: TempReg R: Extra2Reg.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	jump14 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: TempReg.
	self MoveR: TempReg R: Extra2Reg.
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self CmpCq: 2 R: TempReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: TempReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: TempReg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	currentBlock := self Label.
	jump14 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 72 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 128 r: SPReg.
	self MoveR: Extra3Reg Mw: 96 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 104 r: SPReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 112 r: SPReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 120 r: SPReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump15 := self JumpNonZero: 0.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 136 r: SPReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 144 r: SPReg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveMw: 96 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 200 r: SPReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 208 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 216 r: SPReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 224 r: SPReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self CmpCq: 2 R: Extra3Reg.
	jump16 := self JumpLess: 0.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveM64: 0 r: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 152 r: SPReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self AndCq: 16r3FFFFF R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 152 r: SPReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self CmpCq: 31 R: Extra3Reg.
	jump17 := self JumpGreater: 0.
	self MoveMw: 128 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 160 r: SPReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 168 r: SPReg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 176 r: SPReg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 96 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self CmpCq: 31 R: Extra3Reg.
	jump18 := self JumpZero: 0.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self CmpCq: 8 R: Extra3Reg.
	jump19 := self JumpZero: 0.
	currentBlock := self Label.
	jump17 jmpTarget: currentBlock.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self LogicalShiftRightCq: 10 R: Extra2Reg.
	self MoveCw: objectMemory hiddenRootsObject R: SendNumArgsReg.
	self AddCq: 8 R: SendNumArgsReg.
	self LogicalShiftLeftCq: 3 R: Extra2Reg.
	self AddR: Extra2Reg R: SendNumArgsReg.
	self MoveM64: 0 r: SendNumArgsReg R: SendNumArgsReg.
	self CmpCw: objectMemory nilObject R: SendNumArgsReg.
	jump20 := self JumpZero: 0.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self AndCq: 1023 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 152 r: SPReg.
	self AddCq: 8 R: SendNumArgsReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 3 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 152 r: SPReg.
	self MoveMw: 152 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: SendNumArgsReg.
	self MoveM64: 0 r: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 184 r: SPReg.
	jump21 := self Jump: 0.
	currentBlock := self Label.
	jump20 jmpTarget: currentBlock.
	self MoveCw: objectMemory nilObject R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 184 r: SPReg.
	currentBlock := self Label.
	jump21 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 184 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 192 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 184 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 160 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 168 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 176 r: SPReg.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	self MoveMw: 192 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	jump22 := self Jump: 0.
	currentBlock := self Label.
	jump19 jmpTarget: currentBlock.
	self MoveCw: objectMemory nilObject R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 160 r: SPReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 168 r: SPReg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 176 r: SPReg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 96 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	currentBlock := self Label.
	jump18 jmpTarget: currentBlock.
	jump22 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 176 r: SPReg.
	self MoveR: Extra3Reg Mw: 168 r: SPReg.
	self MoveR: Extra3Reg Mw: 160 r: SPReg.
	self MoveMw: 160 r: SPReg R: Extra3Reg.
	self AddCq: 8 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 160 r: SPReg.
	self MoveMw: 160 r: SPReg R: Extra3Reg.
	self AddCq: 16 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 160 r: SPReg.
	self MoveMw: 160 r: SPReg R: Extra3Reg.
	self MoveM64: 0 r: Extra3Reg R: Extra0Reg.
	self LogicalShiftRightCq: 3 R: Extra0Reg.
	self AndCq: 16rFFFF R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 136 r: SPReg.
	self MoveMw: 168 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 144 r: SPReg.
	self MoveMw: 176 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 200 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 208 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 216 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 224 r: SPReg.
	jump23 := self Jump: 0.
	currentBlock := self Label.
	jump15 jmpTarget: currentBlock.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 136 r: SPReg.
	self MoveMw: 120 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 144 r: SPReg.
	self MoveMw: 112 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 104 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveMw: 96 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 200 r: SPReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 208 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 216 r: SPReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 224 r: SPReg.
	currentBlock := self Label.
	jump16 jmpTarget: currentBlock.
	jump23 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 224 r: SPReg.
	self MoveR: Extra3Reg Mw: 216 r: SPReg.
	self MoveR: Extra3Reg Mw: 208 r: SPReg.
	self MoveR: Extra3Reg Mw: 200 r: SPReg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveR: Extra3Reg Mw: 144 r: SPReg.
	self MoveR: Extra3Reg Mw: 136 r: SPReg.
	self MoveMw: 144 r: SPReg R: Extra3Reg.
	self CmpCq: 3 R: Extra3Reg.
	jump24 := self JumpZero: 0.
	self MoveCq: 0 R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 232 r: SPReg.
	self MoveMw: 144 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 240 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 264 r: SPReg.
	self MoveMw: 200 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 288 r: SPReg.
	self MoveMw: 208 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 272 r: SPReg.
	self MoveMw: 136 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 304 r: SPReg.
	self MoveMw: 216 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 296 r: SPReg.
	self MoveMw: 224 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 280 r: SPReg.
	jump25 := self Jump: 0.
	currentBlock := self Label.
	jump24 jmpTarget: currentBlock.
	self MoveMw: 224 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 256 r: SPReg.
	self MoveMw: 216 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 136 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 208 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 200 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 144 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 248 r: SPReg R: Extra3Reg.
	self AndCq: 16r3FFFFF R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveMw: 248 r: SPReg R: Extra3Reg.
	self CmpCq: 36 R: Extra3Reg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: Extra3Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra3Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra3Reg Mw: 248 r: SPReg.
	self MoveMw: 248 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 232 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 240 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 264 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 288 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 272 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 304 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 296 r: SPReg.
	self MoveMw: 256 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 280 r: SPReg.
	currentBlock := self Label.
	jump25 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 280 r: SPReg.
	self MoveR: Extra3Reg Mw: 296 r: SPReg.
	self MoveR: Extra3Reg Mw: 304 r: SPReg.
	self MoveR: Extra3Reg Mw: 272 r: SPReg.
	self MoveR: Extra3Reg Mw: 288 r: SPReg.
	self MoveR: Extra3Reg Mw: 264 r: SPReg.
	self MoveR: Extra3Reg Mw: 240 r: SPReg.
	self MoveR: Extra3Reg Mw: 232 r: SPReg.
	self MoveMw: 232 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump26 := self JumpNonZero: 0.
	self MoveMw: 264 r: SPReg R: Extra3Reg.
	self MoveMw: 304 r: SPReg R: Extra4Reg.
	self SubR: Extra4Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 264 r: SPReg.
	self MoveMw: 240 r: SPReg R: Extra3Reg.
	self CmpCq: 24 R: Extra3Reg.
	jump27 := self JumpGreaterOrEqual: 0.
	self MoveMw: 280 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 312 r: SPReg.
	self MoveMw: 296 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 304 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 272 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 240 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 288 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 272 r: SPReg R: Extra3Reg.
	self CmpCq: 1 R: Extra3Reg.
	jump28 := self JumpGreaterOrEqual: 0.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 320 r: SPReg.
	self MoveMw: 312 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	jump29 := self Jump: 0.
	currentBlock := self Label.
	jump28 jmpTarget: currentBlock.
	self MoveMw: 280 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 296 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 304 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 272 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 240 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveMw: 288 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 272 r: SPReg R: Extra3Reg.
	self MoveMw: 264 r: SPReg R: Extra4Reg.
	self CmpR: Extra4Reg R: Extra3Reg.
	jumpTrue := self JumpLessOrEqual: 0.
	self MoveCq: 0 R: Extra3Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra3Reg.
	jumpNext jmpTarget: self Label.
	self MoveR: Extra3Reg Mw: 272 r: SPReg.
	self MoveMw: 272 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 320 r: SPReg.
	currentBlock := self Label.
	jump29 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 320 r: SPReg.
	self MoveMw: 320 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump30 := self JumpNonZero: 0.
	self CmpCq: 1 R: TempReg.
	jump31 := self JumpLessOrEqual: 0.
	self MoveCq: 4 R: ClassReg.
	jump32 := self Jump: 0.
	currentBlock := self Label.
	jump31 jmpTarget: currentBlock.
	self MoveCq: 2 R: ClassReg.
	currentBlock := self Label.
	jump32 jmpTarget: currentBlock.
	self MoveCq: 0 R: TempReg.
	jump33 := self Jump: 0.
	currentBlock := self Label.
	jump30 jmpTarget: currentBlock.
	self AddR: Extra2Reg R: SendNumArgsReg.
	self CmpCq: 5 R: TempReg.
	jump34 := self JumpLessOrEqual: 0.
	self CmpCq: 16 R: TempReg.
	jump35 := self JumpGreaterOrEqual: 0.
	self CmpCq: 12 R: TempReg.
	jump36 := self JumpGreaterOrEqual: 0.
	self CmpCq: 9 R: TempReg.
	jump37 := self JumpZero: 0.
	self MoveCq: 0 R: ClassReg.
	jump38 := self Jump: 0.
	currentBlock := self Label.
	jump37 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: Arg0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	jump39 := self Jump: 0.
	currentBlock := self Label.
	jump36 jmpTarget: currentBlock.
	self SubCq: 1 R: SendNumArgsReg.
	self AddCq: 8 R: ClassReg.
	self LogicalShiftLeftCq: 1 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self MoveM16: 0 r: ClassReg R: ClassReg.
	self AndCq: 16rFFFF R: ClassReg.
	self LogicalShiftLeftCq: 3 R: ClassReg.
	self AddCq: 1 R: ClassReg.
	jump40 := self Jump: 0.
	currentBlock := self Label.
	jump35 jmpTarget: currentBlock.
	self SubCq: 1 R: SendNumArgsReg.
	self AddCq: 8 R: ClassReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self MoveMw: 0 r: ClassReg R: ClassReg.
	self AndCq: 255 R: ClassReg.
	self LogicalShiftLeftCq: 3 R: ClassReg.
	self AddCq: 1 R: ClassReg.
	jump41 := self Jump: 0.
	currentBlock := self Label.
	jump34 jmpTarget: currentBlock.
	self SubCq: 1 R: SendNumArgsReg.
	self AddCq: 8 R: ClassReg.
	self LogicalShiftLeftCq: 3 R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: ClassReg.
	self MoveM64: 0 r: ClassReg R: ClassReg.
	currentBlock := self Label.
	jump38 jmpTarget: currentBlock.
	jump40 jmpTarget: currentBlock.
	jump41 jmpTarget: currentBlock.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveR: ClassReg R: TempReg.
	self MoveR: SendNumArgsReg R: ClassReg.
	currentBlock := self Label.
	jump33 jmpTarget: currentBlock.
	self CmpCq: 0 R: ClassReg.
	jump42 := self JumpZero: 0.
	self MoveR: Extra1Reg R: Arg0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self CmpCq: 0 R: ClassReg.
	jump43 := self JumpZero: 0.
	jump44 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump42 jmpTarget: currentBlock.
	self MoveR: TempReg R: ReceiverResultReg.
	self CmpCq: 0 R: ClassReg.
	jump45 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump43 jmpTarget: currentBlock.
	self genPrimReturn.
	currentBlock := self Label.
	jump27 jmpTarget: currentBlock.
	self MoveMw: 296 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveMw: 280 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	jump46 := self Jump: 0.
	currentBlock := self Label.
	jump26 jmpTarget: currentBlock.
	self MoveMw: 296 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveMw: 280 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	jump47 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Arg0Reg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ReceiverResultReg.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	jump39 jmpTarget: currentBlock.
	jump44 jmpTarget: currentBlock.
	jump45 jmpTarget: currentBlock.
	jump46 jmpTarget: currentBlock.
	jump47 jmpTarget: currentBlock.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveBitShift [

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self NegateR: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self LogicalShiftRightR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: ClassReg.
	self MoveR: ReceiverResultReg R: TempReg.
	self LogicalShiftLeftR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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
DruidTestRTLCompiler >> gen_primitiveCaseOfOtherwiseValue [

	| jump3 jump1 currentBlock jump2 jump0 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpZero: 0.
	self CmpCq: 1 R: ReceiverResultReg.
	jump1 := self JumpZero: 0.
	self MoveCq: 77 R: ReceiverResultReg.
	jump2 := self Jump: 0.
	
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 42 R: ReceiverResultReg.
	jump3 := self Jump: 0.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 57 R: ReceiverResultReg.
	
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	jump3 jmpTarget: currentBlock.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| jump0 jump1 currentBlock |
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
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
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMaskInverted [

	| jump0 jump1 currentBlock |
	self TstCq: 1 R: ReceiverResultReg.
	jump0 := self JumpNonZero: 0.
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
	self MoveCq: 99 R: TempReg.
	self CmpCq: 0 R: ReceiverResultReg.
	jump1 := self JumpGreater: 0.
	self MoveCq: 99 R: TempReg.
	self MoveCq: 17 R: ClassReg.
	jump2 := self Jump: 0.
	
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self MoveCq: 5 R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra0Reg.
	self MoveR: TempReg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self CmpCq: 7 R: ReceiverResultReg.
	jump3 := self JumpGreater: 0.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self MoveR: Extra2Reg R: ClassReg.
	
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self AddR: ClassReg R: TempReg.
	self AddCq: 42 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	jump4 := self Jump: 0.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self MoveCq: 5 R: Extra2Reg.
	self MoveR: ClassReg R: Extra0Reg.
	self MoveR: TempReg R: Extra1Reg.
	
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	self AddCq: 5 R: Extra0Reg.
	self AddCq: 15 R: Extra0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	
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

	| jump0 jump1 currentBlock |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpGreater: 0.
	self MoveR: ReceiverResultReg R: TempReg.
	self NegateR: TempReg.
	self MoveCq: 1 R: ClassReg.
	self LogicalShiftRightR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveCq: 1 R: ClassReg.
	self MoveR: ReceiverResultReg R: TempReg.
	self LogicalShiftLeftR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
	
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
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
	self TstCq: 1 R: ReceiverResultReg.
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
		Quo: TempReg
		Rem: ClassReg.
	self MoveR: ClassReg R: ReceiverResultReg.
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

	| jump0 jump1 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self LogicalShiftRightCq: 3 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self SubCq: 1 R: ClassReg.
	self MulR: ClassReg R: TempReg.
	jump0 := self JumpMultiplyOverflow: 0.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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

	| jump5 jump17 jump10 jump3 jump15 jump8 jump1 currentBlock jump13 jumpNext jump6 jump18 jump11 jump20 jump4 jumpTrue jump16 jump9 jump2 b328 jump14 jump7 jump0 jump19 jump12 jump21 |
	self AddCq: 56 R: SPReg.
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
	jump0 := self JumpLessOrEqual: 0.
	self MoveR: TempReg R: SendNumArgsReg.
	self CmpCq: 5 R: SendNumArgsReg.
	jumpTrue := self JumpZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: SendNumArgsReg.
	jumpNext jmpTarget: self Label.
	
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self CmpCq: 0 R: SendNumArgsReg.
	jump1 := self JumpNonZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	jump2 := self Jump: 0.
	
	currentBlock := self Label.
	jump1 jmpTarget: currentBlock.
	self MoveCq: 4 R: Extra0Reg.
	self MoveR: ReceiverResultReg R: SendNumArgsReg.
	self AddR: SendNumArgsReg R: Extra0Reg.
	self MoveM32: 0 r: Extra0Reg R: SendNumArgsReg.
	self AndCq: 16rFFFFFFFF R: SendNumArgsReg.
	self AndCq: 16r3FFFFF R: SendNumArgsReg.
	self CmpCq: 0 R: SendNumArgsReg.
	jump3 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self MoveR: TempReg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self CmpCq: 0 R: SendNumArgsReg.
	jump4 := self JumpLess: 0.
	self AndCq: 16rFFFF R: ClassReg.
	self CmpCq: 255 R: ClassReg.
	jump5 := self JumpGreaterOrEqual: 0.
	self CmpCq: 1 R: ClassReg.
	jump6 := self JumpLess: 0.
	self MoveCq: 8 R: Extra2Reg.
	self MulR: ClassReg R: Extra2Reg.
	jump7 := self Jump: 0.
	
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveCq: 8 R: Extra2Reg.
	
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: ClassReg R: Extra0Reg.
	self AddCq: 8 R: Extra2Reg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	jump8 := self Jump: 0.
	
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveR: ClassReg R: TempReg.
	self LogicalShiftRightCq: 56 R: TempReg.
	self CmpCq: 0 R: TempReg.
	jump9 := self JumpGreater: 0.
	self MoveCq: 8 R: TempReg.
	self MulR: ClassReg R: TempReg.
	self AddCq: 16 R: TempReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: ClassReg R: Extra3Reg.
	
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	self MoveAw: objectMemory freeStartAddress R: SendNumArgsReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self CmpCw: objectMemory getScavengeThreshold R: Extra2Reg.
	jumpTrue := self JumpGreater: 0.
	self MoveCq: 0 R: Extra2Reg.
	jumpNext := self Jump: 0.
	jumpTrue jmpTarget: self Label.
	self MoveCq: 1 R: Extra2Reg.
	jumpNext jmpTarget: self Label.
	self CmpCw: objectMemory getScavengeThreshold R: SendNumArgsReg.
	jump10 := self JumpGreater: 0.
	self MoveAw: objectMemory freeStartAddress R: Extra2Reg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: TempReg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self CmpCq: 255 R: Extra3Reg.
	jump11 := self JumpGreaterOrEqual: 0.
	jump12 := self JumpLess: 0.
	
	currentBlock := self Label.
	jump10 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra2Reg.
	jump13 := self JumpNonZero: 0.
	self MoveAw: objectMemory freeStartAddress R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: ClassReg.
	self MoveR: SendNumArgsReg R: TempReg.
	self MoveR: ClassReg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	self MoveR: ClassReg R: SendNumArgsReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self CmpCq: 255 R: Extra3Reg.
	jump14 := self JumpGreaterOrEqual: 0.
	
	currentBlock := self Label.
	jump12 jmpTarget: currentBlock.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 56 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveMw: 32 r: SPReg R: Extra4Reg.
	self AddR: Extra4Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveMw: 16 r: SPReg R: Extra4Reg.
	self AddR: Extra4Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self OrCq: 0 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg M64: 0 r: Extra2Reg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	jump15 := self Jump: 0.
	
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	jump14 jmpTarget: currentBlock.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self AddCq: 16rFF00000000000000 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveMw: 40 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg M64: 0 r: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 8 R: ClassReg.
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
	self MoveR: Extra3Reg M64: 0 r: ClassReg.
	self AddCq: 8 R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	
	currentBlock := self Label.
	jump15 jmpTarget: currentBlock.
	self MoveAw: objectMemory freeStartAddress R: TempReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: TempReg.
	self MoveR: TempReg Aw: objectMemory freeStartAddress.
	self MoveR: Extra0Reg R: TempReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self CmpCq: 0 R: Extra3Reg.
	jump16 := self JumpZero: 0.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self AddCq: 8 R: TempReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self AddCq: 8 R: SendNumArgsReg.
	self MoveCq: 8 R: Extra2Reg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MulR: Extra3Reg R: Extra2Reg.
	self AddR: Extra2Reg R: SendNumArgsReg.
	self SubCq: 1 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
	
	b328 := self Label.
	self CmpR: TempReg R: Extra2Reg.
	jump17 := self JumpGreaterOrEqual: 0.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: ClassReg.
	jump18 := self Jump: 0.
	
	currentBlock := self Label.
	jump17 jmpTarget: currentBlock.
	self MoveCw: 0 R: ClassReg.
	self MoveR: ClassReg M64: 0 r: TempReg.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 8 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	jump19 := self Jump: b328.
	
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	jump20 := self Jump: 0.
	
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: ClassReg.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	self MoveR: ClassReg R: TempReg.
	
	currentBlock := self Label.
	jump16 jmpTarget: currentBlock.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MoveR: TempReg R: ClassReg.
	
	currentBlock := self Label.
	jump18 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: SendNumArgsReg.
	self MoveR: ClassReg R: Extra0Reg.
	
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self CmpCq: 0 R: SendNumArgsReg.
	jump21 := self JumpZero: 0.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self genPrimReturn.
	
	currentBlock := self Label.
	jump21 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	
	currentBlock := self Label.
	jump3 jmpTarget: currentBlock.
	jump4 jmpTarget: currentBlock.
	jump20 jmpTarget: currentBlock.
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

	| jump0 jump1 currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	jump0 := self JumpOverflow: 0.
	self MoveR: TempReg R: ReceiverResultReg.
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
DruidTestRTLCompiler >> gen_primitiveUint32At [

	| currentBlock |
	self MoveM32: 0 r: ReceiverResultReg R: TempReg.
	self AndCq: 16rFFFFFFFF R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
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
