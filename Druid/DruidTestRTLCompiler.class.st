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

	| currentBlock jump0 jump1 |
	self mclassIsSmallInteger ifFalse: [ ^ UnimplementedPrimitive ].
	self TstCq: 1 R: Arg0Reg.
	jump0 := self JumpZero: 0.
	self MoveR: Arg0Reg R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: ReceiverResultReg R: ClassReg.
	self AddR: ClassReg R: TempReg.
	jump1 := self JumpOverflow: 0.
	self MoveR: TempReg R: Arg0Reg.
	self genPrimReturn.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
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

	| currentBlock jump0 jump1 jump2 jump3 jump4 jump5 jump6 jump7 jump8 jump9 jump10 jump11 jump12 jump13 jump14 jump15 jump16 jump17 jump18 jump19 jump20 jump21 jump22 jump23 jump24 jump25 jump26 jump27 jump28 jump29 jump30 jump31 jump32 jump33 jump34 jump35 jump36 jump37 jump38 jump39 jump40 jump41 jump42 jump43 jump44 jump45 jump46 jump47 jump48 jump49 |
	self AddCq: 96 R: SPReg.
	self TstCq: 7 R: Arg0Reg.
	jump0 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Arg1Reg.
	self MoveR: Arg1Reg R: Arg0Reg.
	self MoveR: Arg0Reg R: ReceiverResultReg.
	jump1 := self Jump: 0.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: TempReg.
	self AndCq: 1 R: TempReg.
	self CmpCq: 0 R: TempReg.
	self OrCq: 0 R: TempReg.
	jump2 := self JumpZero: 0.
	self MoveR: ReceiverResultReg R: Arg1Reg.
	self MoveR: Arg1Reg R: Arg0Reg.
	self MoveR: Arg0Reg R: ReceiverResultReg.
	jump3 := self Jump: 0.
	currentBlock := self Label.
	jump2 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: TempReg.
	self LogicalShiftLeftCq:  -63 R: TempReg.
	self CmpCq: 1 R: TempReg.
	jump4 := self JumpNonZero: 0.
	self MoveR: Arg0Reg R: TempReg.
	self LogicalShiftLeftCq:  -3 R: TempReg.
	self AndCq: 16r1FFFFFFFFFFFFFFF R: TempReg.
	self SubCq: 16r1FFFFFFFFFFFFFFF R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	jump5 := self Jump: 0.
	currentBlock := self Label.
	jump4 jmpTarget: currentBlock.
	self MoveR: Arg0Reg R: TempReg.
	self LogicalShiftLeftCq:  -3 R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	currentBlock := self Label.
	jump5 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Arg0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self LogicalShiftRightCq: 24 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self AndCq: 31 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 24 r: SPReg.
	self MoveR: Arg0Reg R: Extra0Reg.
	self AddCq: 7 R: Extra0Reg.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self CmpCq: 255 R: Extra0Reg.
	jump6 := self JumpNonZero: 0.
	self MoveR: Arg0Reg R: Extra0Reg.
	self SubCq: 8 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self LogicalShiftLeftR: Extra0Reg Cq: 8.
	self LogicalShiftRightCq: 8 R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra0Reg.
	jump7 := self Jump: 0.
	currentBlock := self Label.
	jump6 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra0Reg.
	currentBlock := self Label.
	jump7 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra1Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 5 R: Extra3Reg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	jump8 := self JumpLessOrEqual: 0.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 16 R: Extra3Reg.
	jump9 := self JumpLess: 0.
	self LogicalShiftLeftR: Extra0Reg Cq: 3.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self AndCq: 7 R: Extra1Reg.
	self SubR: Extra1Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	jump10 := self Jump: 0.
	currentBlock := self Label.
	jump9 jmpTarget: currentBlock.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 12 R: Extra3Reg.
	jump11 := self JumpLess: 0.
	self LogicalShiftLeftR: Extra0Reg Cq: 2.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self AndCq: 3 R: Extra1Reg.
	self SubR: Extra1Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	jump12 := self Jump: 0.
	currentBlock := self Label.
	jump11 jmpTarget: currentBlock.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 10 R: Extra3Reg.
	jump13 := self JumpLess: 0.
	self LogicalShiftLeftR: Extra0Reg Cq: 1.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self AndCq: 1 R: Extra1Reg.
	self SubR: Extra1Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	jump14 := self Jump: 0.
	currentBlock := self Label.
	jump13 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra1Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	jump15 := self JumpZero: 0.
	self MoveCq: 0 R: Extra1Reg.
	self MoveR: Extra1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	currentBlock := self Label.
	jump8 jmpTarget: currentBlock.
	jump10 jmpTarget: currentBlock.
	jump12 jmpTarget: currentBlock.
	jump14 jmpTarget: currentBlock.
	jump15 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveR: ReceiverResultReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 56 r: SPReg.
	self MoveR: Arg1Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 48 r: SPReg.
	self MoveR: Arg0Reg R: TempReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 2 R: Extra3Reg.
	self OrR: SendNumArgsReg R: ClassReg.
	jump16 := self JumpZero: 0.
	self MoveCq: 0 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 72 r: SPReg.
	jump17 := self Jump: 0.
	currentBlock := self Label.
	jump16 jmpTarget: currentBlock.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 2 R: Extra3Reg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: TempReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 72 r: SPReg.
	jump18 := self JumpLess: 0.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self MoveR: Arg0Reg R: Extra1Reg.
	self AndCq: 16r3FFFFF R: Extra1Reg.
	self MoveR: Extra1Reg R: ClassReg.
	self CmpCq: 31 R: Extra1Reg.
	self MoveR: ClassReg R: ClassReg.
	jump19 := self JumpGreater: 0.
	self CmpCq: 31 R: Extra1Reg.
	self MoveR: SendNumArgsReg R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	jump20 := self JumpZero: 0.
	self CmpCq: 8 R: Extra1Reg.
	jump21 := self JumpNonZero: 0.
	self MoveCw: objectMemory nilObject R: Extra0Reg.
	self MoveR: Extra0Reg R: ClassReg.
	self MoveR: TempReg R: SendNumArgsReg.
	self MoveMw: 48 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 56 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	jump22 := self Jump: 0.
	currentBlock := self Label.
	jump21 jmpTarget: currentBlock.
	self MoveR: Extra1Reg R: Extra0Reg.
	self MoveR: Extra0Reg R: ClassReg.
	currentBlock := self Label.
	jump19 jmpTarget: currentBlock.
	self MoveR: ClassReg R: Extra0Reg.
	self LogicalShiftRightCq: 10 R: Extra0Reg.
	self MoveCw: objectMemory nilObject R: TempReg.
	self AddCq: 8 R: TempReg.
	self LogicalShiftLeftR: Extra0Reg Cq: 3.
	self AddR: Extra0Reg R: TempReg.
	self MoveR: TempReg R: TempReg.
	self CmpCw: objectMemory nilObject R: TempReg.
	jump23 := self JumpNonZero: 0.
	self MoveCw: objectMemory nilObject R: TempReg.
	self MoveR: TempReg R: TempReg.
	jump24 := self Jump: 0.
	currentBlock := self Label.
	jump23 jmpTarget: currentBlock.
	self AndCq: 1023 R: ClassReg.
	self AddCq: 8 R: TempReg.
	self LogicalShiftLeftR: ClassReg Cq: 3.
	self AddR: ClassReg R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: TempReg.
	self MoveR: TempReg R: TempReg.
	currentBlock := self Label.
	jump24 jmpTarget: currentBlock.
	self MoveR: ReceiverResultReg R: Extra0Reg.
	self MoveR: Arg1Reg R: Extra2Reg.
	self MoveR: Arg0Reg R: SendNumArgsReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self MoveR: Extra0Reg R: Extra0Reg.
	currentBlock := self Label.
	jump20 jmpTarget: currentBlock.
	jump22 jmpTarget: currentBlock.
	self AddCq: 8 R: ClassReg.
	self AddCq: 16 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self LogicalShiftLeftCq:  -63 R: ClassReg.
	self CmpCq: 1 R: ClassReg.
	jump25 := self JumpNonZero: 0.
	self LogicalShiftLeftCq:  -3 R: TempReg.
	self AndCq: 16r1FFFFFFFFFFFFFFF R: TempReg.
	self SubCq: 16r1FFFFFFFFFFFFFFF R: TempReg.
	self SubCq: 1 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	jump26 := self Jump: 0.
	currentBlock := self Label.
	jump25 jmpTarget: currentBlock.
	self LogicalShiftLeftCq:  -3 R: TempReg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	currentBlock := self Label.
	jump26 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self AndCq: 16rFFFF R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	self MoveR: Extra0Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 72 r: SPReg.
	currentBlock := self Label.
	jump17 jmpTarget: currentBlock.
	jump18 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 40 r: SPReg.
	self MoveR: Extra3Reg Mw: 80 r: SPReg.
	self MoveR: Extra3Reg Mw: 64 r: SPReg.
	self MoveR: Extra3Reg Mw: 72 r: SPReg.
	self MoveCq: 0 R: Extra2Reg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 3 R: Extra3Reg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self AndCq: 16r3FFFFF R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 16 r: SPReg.
	self MoveMw: 16 r: SPReg R: Extra3Reg.
	self CmpCq: 36 R: Extra3Reg.
	self AndR: TempReg R: Extra0Reg.
	self MoveR: Extra2Reg R: TempReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra2Reg.
	jump27 := self JumpNonZero: 0.
	self MoveMw: 32 r: SPReg R: Extra3Reg.
	self MoveMw: 40 r: SPReg R: Extra4Reg.
	self SubR: Extra4Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 32 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 24 R: Extra3Reg.
	jump28 := self JumpLess: 0.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	jump29 := self Jump: 0.
	currentBlock := self Label.
	jump28 jmpTarget: currentBlock.
	self MoveCq: 1 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	currentBlock := self Label.
	jump29 jmpTarget: currentBlock.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveCq: 0 R: TempReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self CmpR: Extra2Reg R: Extra3Reg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveMw: 32 r: SPReg R: Extra4Reg.
	self CmpR: Extra4Reg R: Extra3Reg.
	self AndR: ClassReg R: Extra2Reg.
	jump30 := self JumpZero: 0.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self MoveMw: 40 r: SPReg R: Extra4Reg.
	self AddR: Extra4Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 5 R: Extra3Reg.
	jump31 := self JumpLessOrEqual: 0.
	jump32 := self JumpGreater: 0.
	currentBlock := self Label.
	jump30 jmpTarget: currentBlock.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 1 R: Extra3Reg.
	jump33 := self JumpLessOrEqual: 0.
	jump34 := self JumpGreater: 0.
	currentBlock := self Label.
	jump31 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self SubCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Arg0Reg R: ClassReg.
	self AddCq: 8 R: ClassReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftR: Extra3Reg Cq: 3.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	jump35 := self Jump: 0.
	currentBlock := self Label.
	jump32 jmpTarget: currentBlock.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 16 R: Extra3Reg.
	jump36 := self JumpGreaterOrEqual: 0.
	jump37 := self JumpLess: 0.
	currentBlock := self Label.
	jump33 jmpTarget: currentBlock.
	self MoveCq: 2 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	jump38 := self Jump: 0.
	currentBlock := self Label.
	jump34 jmpTarget: currentBlock.
	self MoveCq: 4 R: SendNumArgsReg.
	self MoveR: SendNumArgsReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	jump39 := self Jump: 0.
	currentBlock := self Label.
	jump36 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self SubCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Arg0Reg R: ClassReg.
	self AddCq: 8 R: ClassReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: ClassReg.
	self MoveCq: 0 R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	self LogicalShiftLeftR: ClassReg Cq: 3.
	self AddCq: 1 R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	self MoveR: ClassReg R: ClassReg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	jump40 := self Jump: 0.
	currentBlock := self Label.
	jump37 jmpTarget: currentBlock.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 12 R: Extra3Reg.
	jump41 := self JumpGreaterOrEqual: 0.
	jump42 := self JumpLess: 0.
	currentBlock := self Label.
	jump38 jmpTarget: currentBlock.
	jump39 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 88 r: SPReg.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: ClassReg.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra0Reg.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 96 r: SPReg.
	self MoveR: Extra0Reg R: SendNumArgsReg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self MoveR: ClassReg R: ClassReg.
	self MoveMw: 88 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	jump43 := self Jump: 0.
	currentBlock := self Label.
	jump41 jmpTarget: currentBlock.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self SubCq: 1 R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveR: Arg0Reg R: Extra2Reg.
	self AddCq: 8 R: Extra2Reg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self LogicalShiftLeftR: Extra3Reg Cq: 1.
	self MoveR: Extra3Reg Mw: 8 r: SPReg.
	self MoveMw: 8 r: SPReg R: Extra3Reg.
	self AddR: Extra3Reg R: Extra2Reg.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self LogicalShiftLeftR: Extra2Reg Cq: 3.
	self AddCq: 1 R: Extra2Reg.
	self MoveR: Extra2Reg R: Extra2Reg.
	self MoveR: Extra2Reg R: ClassReg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	jump44 := self Jump: 0.
	currentBlock := self Label.
	jump42 jmpTarget: currentBlock.
	self MoveMw: 24 r: SPReg R: Extra3Reg.
	self CmpCq: 9 R: Extra3Reg.
	self MoveR: TempReg R: ClassReg.
	self MoveR: Extra0Reg R: Extra0Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	jump45 := self JumpZero: 0.
	self MoveMw: 80 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra2Reg.
	self MoveMw: 64 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: SendNumArgsReg.
	self MoveMw: 72 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: Extra1Reg.
	self MoveCq: 0 R: Extra0Reg.
	self MoveR: Extra0Reg R: ClassReg.
	self MoveR: Extra1Reg R: Extra0Reg.
	self MoveR: SendNumArgsReg R: Extra1Reg.
	self MoveR: Extra2Reg R: SendNumArgsReg.
	currentBlock := self Label.
	jump35 jmpTarget: currentBlock.
	jump40 jmpTarget: currentBlock.
	jump44 jmpTarget: currentBlock.
	jump45 jmpTarget: currentBlock.
	self MoveCq: 0 R: Extra2Reg.
	self MoveR: ClassReg R: Extra3Reg.
	self MoveR: Extra3Reg Mw: 96 r: SPReg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self MoveR: Extra0Reg R: ClassReg.
	self MoveR: Extra2Reg R: Extra2Reg.
	currentBlock := self Label.
	jump43 jmpTarget: currentBlock.
	self MoveR: Extra3Reg Mw: 96 r: SPReg.
	self MoveMw: 96 r: SPReg R: Extra3Reg.
	self MoveR: Extra3Reg R: TempReg.
	self MoveR: ClassReg R: Extra0Reg.
	self MoveR: Extra1Reg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: SendNumArgsReg.
	self MoveR: Extra2Reg R: Extra2Reg.
	currentBlock := self Label.
	jump27 jmpTarget: currentBlock.
	self CmpCq: 0 R: Extra2Reg.
	jump46 := self JumpNonZero: 0.
	self MoveR: TempReg R: Arg1Reg.
	self MoveR: Extra1Reg R: Arg0Reg.
	self MoveR: Extra0Reg R: ReceiverResultReg.
	self CmpCq: 0 R: Extra2Reg.
	jump47 := self JumpZero: 0.
	jump48 := self JumpNonZero: 0.
	currentBlock := self Label.
	jump46 jmpTarget: currentBlock.
	self MoveR: Extra0Reg R: Arg1Reg.
	self MoveR: Extra1Reg R: Arg0Reg.
	self MoveR: SendNumArgsReg R: ReceiverResultReg.
	self CmpCq: 0 R: Extra2Reg.
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
DruidTestRTLCompiler >> gen_primitiveConditionWithAnyMask [

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 jump2 jump3 jump4 |
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
	self CmpCq: 7 R: ReceiverResultReg.
	self MoveR: ClassReg R: Extra0Reg.
	self MoveR: TempReg R: Extra1Reg.
	self MoveR: SendNumArgsReg R: Extra2Reg.
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
	self CmpCq: 7 R: ReceiverResultReg.
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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
DruidTestRTLCompiler >> gen_primitiveFalseAsInteger [

	| currentBlock |
	self MoveCq: 0 R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveGreaterOrEqualsThan [

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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
DruidTestRTLCompiler >> gen_primitiveIfTrueIfFalseAssigningValue [

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
DruidTestRTLCompiler >> gen_primitiveIfTrueIfFalseReturningValue [

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
DruidTestRTLCompiler >> gen_primitiveIfTrueStatement [

	| currentBlock jump0 jump1 |
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
DruidTestRTLCompiler >> gen_primitiveIncrement [

	| currentBlock |
	self MoveR: ReceiverResultReg R: TempReg.
	self AddCq: 1 R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveIsIntegerObject [

	| currentBlock jump0 jump1 |
	self MoveR: ReceiverResultReg R: TempReg.
	self TstCq: 1 R: TempReg.
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
DruidTestRTLCompiler >> gen_primitiveLessOrEqualsThan [

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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
DruidTestRTLCompiler >> gen_primitiveLoopIncrementing [

	| currentBlock b15 jump0 |
	self MoveCq: 10 R: TempReg.
	b15 := self Label.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
	self CmpCq: 15 R: ClassReg.
	jump0 := self JumpLess: b15.
	self MoveR: ClassReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveLoopIncrementingGlobalState [

	| currentBlock b14 jump0 |
	self MoveCq: 10 R: TempReg.
	b14 := self Label.
	self MoveR: TempReg R: ClassReg.
	self AddCq: 1 R: ClassReg.
	self MoveR: ClassReg R: TempReg.
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
DruidTestRTLCompiler >> gen_primitiveMultiplyWithOverflow [

	| currentBlock jump0 |
	self MoveR: ReceiverResultReg R: TempReg.
	self LogicalShiftRightCq: 3 R: TempReg.
	self MoveR: Arg0Reg R: ClassReg.
	self SubCq: 1 R: ClassReg.
	self MulR: TempReg R: ClassReg.
	jump0 := self JumpMultiplyOverflow: 0.
	self AddCq: 1 R: ClassReg.
	self MoveR: ClassReg R: Arg0Reg.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
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
DruidTestRTLCompiler >> gen_primitiveNotEqualsThan [

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 jump2 |
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

	| currentBlock jump0 jump1 |
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
DruidTestRTLCompiler >> gen_primitiveUint16At [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveM16: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint32At [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveM32: 0 r: ReceiverResultReg R: TempReg.
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
	self MoveR: Arg0Reg M64: 0 r: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveUint8At [

	| currentBlock |
	self MoveCq: 0 R: TempReg.
	self MoveMb: 0 r: ReceiverResultReg R: TempReg.
	self MoveR: TempReg R: ReceiverResultReg.
	self genPrimReturn.
	^ CompletePrimitive
]

{ #category : #generated }
DruidTestRTLCompiler >> gen_primitiveWithDeadCode [

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 jump1 |
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

	| currentBlock jump0 |
	self CmpCq: 0 R: ReceiverResultReg.
	jump0 := self JumpLessOrEqual: 0.
	self MoveCq: 11 R: ReceiverResultReg.
	self genPrimReturn.
	currentBlock := self Label.
	jump0 jmpTarget: currentBlock.
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
