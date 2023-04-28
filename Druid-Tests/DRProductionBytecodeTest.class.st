Class {
	#name : #DRProductionBytecodeTest,
	#superclass : #DRBytecodeCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeAdd [

	"We do not support the static type prediction yet"
	self skip.

	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 1 put: sendTrampolineAddress.
	
	self
		compileBytecode: 96
		selector: #bytecodePrimAdd
		thenDo: [ :generator | 
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: { (memory integerObjectOf: 42) }.
	self
		assert:
		(memory integerValueOf: machineSimulator receiverRegisterValue)
		equals: 42 + 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeAddOverflow [

	sendTrampolineAddress := self compile: [ "In case of overflow, this routine is called and returns the receiver unchanged"
		cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 1 put: sendTrampolineAddress.

	self
		compileBytecode: 96
		selector: #bytecodePrimAdd
		thenDo: [ :generator | 
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushRegister: Arg0Reg.

			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	"Should run, overflow, jump to the trampoline, and 17"
	self
		executePrimitiveWithReceiver:
		(memory integerObjectOf: memory maxSmallInteger)
		withArguments: { (memory integerObjectOf: 1) }.
	
	self
		assert: (memory integerValueOf: machineSimulator receiverRegisterValue)
		equals: memory maxSmallInteger
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeDuplicateTop [

	self compileBytecode: 83 selector: #duplicateTopBytecode thenDo: [ :generator |
		cogit ssPushRegister: TempReg.
		
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop moveToReg: ReceiverResultReg.
		cogit ssPop: 2.
		cogit genUpArrowReturn ].

	machineSimulator temporaryRegisterValue: 17.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 42).
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePrimSendAdd [

	"Setup the send trampoline"
	| method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 1 "arg" put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	self
		compileBytecode: 96
		selector: #bytecodePrimAdd
		thenDo: [ :generator | 
			cogit methodObj: method.
			"The receiver of the send!"
			cogit ssPushRegister: ReceiverResultReg.
			cogit ssPushConstant: (memory integerObjectOf: 42).

			"Execute the druid's compiled code"
			generator value.

			cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: initialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: (-1 twoComplementOfBitSize: 64) "+ selector index".
	"Third argument: the receiver, then the argument is on the stack"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17).
	self assert: machineSimulator arg0RegisterValue equals: (memory integerObjectOf: 42)
	



]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushConstantOneBytecode [

	| method |
	method := methodBuilder
		          newMethod
		          buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 81
		selector: #pushConstantOneBytecode
		thenDo: [ :generator | 
			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 1)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushConstantZeroBytecode [

	| method |
	method := methodBuilder
		          newMethod
		          buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 80
		selector: #pushConstantZeroBytecode
		thenDo: [ :generator | 
			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 0)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant [

	| literalIndex method |
	method := methodBuilder
		          newMethod
		          literals: { (memory integerObjectOf: 42) };
		          buildMethod.

	cogit methodObj: method.

	literalIndex := 0. "0 based..."
	self
		compileBytecode: 32 + literalIndex
		selector: #pushLiteralConstantBytecode
		thenDo: [ :generator | 
			"Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).
		
	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 42)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralConstant2 [

	| literalIndex method |

	method := methodBuilder
		          newMethod
		          literals: { (memory integerObjectOf: 42). (memory integerObjectOf: 43) };
		          buildMethod.
		
	cogit methodObj: method.

	literalIndex := 1 "0 based...".
	self compileBytecode: 32 + literalIndex selector: #pushLiteralConstantBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].


	self executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #().
		
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 43)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable0 [

	self testBytecodePushLiteralVariable: 0
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable1 [

	self testBytecodePushLiteralVariable: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable15 [

	self testBytecodePushLiteralVariable: 15
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushLiteralVariable: tempIndex [

	| method |
	method := methodBuilder newMethod
		          literals:
			          (((1 to: 16) collect: [ :e | nil -> (e + 16rFF - 1) ])
				           collect: [ :e | e forMemory: memory inMethod: nil ]);
		          buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 16 + tempIndex
		selector: #pushLiteralVariable16CasesBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: tempIndex + 16rFF)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg0NoTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 0;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 0. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 1.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg0WithTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 1;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 0. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg1NoTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 2;
		          numberOfTemporaries: 0;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 2.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 43 ).

	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryArg1WithTemps [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 2;
		          numberOfTemporaries: 2;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 2.
			cogit methodOrBlockNumTemps: 4.
			cogit initSimStackForFramefulMethod: 2.

			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 43 ).

	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp0NoArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 0;
		          numberOfTemporaries: 1;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 0. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 0.
			cogit methodOrBlockNumTemps: 1.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 42 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp0WithArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 1;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 5 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp1NoArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 0;
		          numberOfTemporaries: 2;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 1. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 0.
			cogit methodOrBlockNumTemps: 2.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 42 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodePushTemporaryTemp1WithArgs [

	| tempIndex method |
	method := methodBuilder newMethod
		          numberOfArguments: 1;
		          numberOfTemporaries: 2;
		          buildMethod.

	cogit methodObj: method.

	tempIndex := 2. "0 based..."
	self
		compileBytecode: 64 + tempIndex
		selector: #pushTemporaryVariableBytecode
		thenDo: [ :generator | "Execute the druid's compiled code"
			cogit methodOrBlockNumArgs: 1.
			cogit methodOrBlockNumTemps: 3.
			cogit initSimStackForFramefulMethod: 2.

			"Set temporary value on frame"
			cogit MoveCq: 5 R: TempReg.
			cogit
				MoveR: TempReg
				Mw: (cogit frameOffsetOfTemporary: tempIndex)
				r: FPReg.
				
			generator value.

			"Then return without druid's compiled code"
			cogit ssTop popToReg: ReceiverResultReg.
			cogit genUpArrowReturn ].

	self
		executeMethodWithReceiver: (memory integerObjectOf: 17)
		withArguments: #( 42 ).

	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeReturnFalse [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 90
		selector: #returnFalse
		thenDo: [ :generator | "Execute the druid's compiled code"
		generator value ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory falseObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeReturnNil [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 91
		selector: #returnNil
		thenDo: [ :generator | "Execute the druid's compiled code"
		generator value ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory nilObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeReturnTrue [

	| method |
	method := methodBuilder newMethod buildMethod.

	cogit methodObj: method.

	self
		compileBytecode: 89
		selector: #returnTrue
		thenDo: [ :generator | "Execute the druid's compiled code"
		generator value ].

	self
		executePrimitiveWithReceiver: (memory integerObjectOf: 17)
		withArguments: #(  ).

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeSendLiteral0ToSelf [

	"128 - sendLiteralSelector0ArgsBytecode selector=literal0, numargs=0.
	Test we do a send and we end up in a trampoline"
	
	"Setup the send trampoline"
	| method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 0 put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	self
		compileBytecode: 128 + 0 "selector index"
		selector: #sendLiteralSelector0ArgsBytecode
		thenDo: [ :generator | 
			cogit methodObj: method.
			"The receiver of the send!"
			cogit ssPushRegister: ReceiverResultReg.

			"Execute the druid's compiled code"
			generator value.

			cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: initialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: 0 "selector index".
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeSendLiteral1ToSelf [

	"129 - sendLiteralSelector0ArgsBytecode selector=literal0, numargs=0.
	Test we do a send and we end up in a trampoline"
	
	"Setup the send trampoline"
	| method selector |
	sendTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ordinarySendTrampolineAt: 0 put: sendTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { 
			memory nilObject.
			selector := (memory integerObjectOf: 42) };
		buildMethod.

	self
		compileBytecode: 128 + 1 "selector index"
		selector: #sendLiteralSelector0ArgsBytecode
		thenDo: [ :generator | 
			cogit methodObj: method.
			"The receiver of the send!"
			cogit ssPushRegister: ReceiverResultReg.

			"Execute the druid's compiled code"
			generator value.

			cogit Stop ].

	self
		prepareStackForPrimitiveReceiver: (memory integerObjectOf: 17)
		arguments: #()
		method: method.

	"Check it arrives to trampoline ceSend: ClassReg above: true to: ReceiverReg numArgs: numArgs"
	self runFrom: initialAddress until: sendTrampolineAddress.
	"First argument: the selector"
	self assert: machineSimulator classRegisterValue equals: 1 "selector index".
	"Third argument: the receiver"
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 17)
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeShortJumpIfFalseWithFalse [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push false"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 192
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	"self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
	self runFrom: initialAddress until: mustBeBooleanTrampolineAddress."
	self executePrimitiveWithReceiver: memory falseObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeShortJumpIfFalseWithTrue [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push false"
			192 "jump if false over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 192
		selector: #shortConditionalJumpFalse
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	"self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
	self runFrom: initialAddress until: mustBeBooleanTrampolineAddress."
	self executePrimitiveWithReceiver: memory trueObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeShortJumpIfTrueWithFalse [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push true"
			184 "jump if true over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	"self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
	self runFrom: initialAddress until: mustBeBooleanTrampolineAddress."
	self executePrimitiveWithReceiver: memory falseObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeShortJumpIfTrueWithTrue [

	| method mustBeBooleanTrampolineAddress |
	
	mustBeBooleanTrampolineAddress := self compile: [ cogit RetN: 0 ].
	cogit ceSendMustBeBooleanTrampoline: mustBeBooleanTrampolineAddress.
	
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 
			77 "push true"
			184 "jump if true over next"
			90 "return false"
			91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 184
		selector: #shortConditionalJumpTrue
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).

			cogit bytecodePC: 16.
			cogit ssPushRegister: ReceiverResultReg.
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 17.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 18.
			cogit ssPushConstant: 0.
			cogit genReturnTopFromMethod.
			cogit Stop.

			
			cogit bytecodePC: 19.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 19) nextOpcodeIndex: cogit opcodeIndex.
			cogit ssPushConstant: 1.
			cogit genReturnTopFromMethod. ].

	"self
		prepareStackForSendReceiver: memory trueObject
		arguments: #()
		method: method.
	self runFrom: initialAddress until: mustBeBooleanTrampolineAddress."
	self executePrimitiveWithReceiver: memory trueObject.

	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #tests }
DRProductionBytecodeTest >> testBytecodeShortUnconditionalJump [

	| method |
	method := methodBuilder
		newMethod;
		literals: { (memory integerObjectOf: 42) };
		bytecodes: #[ 176 90 "return false" 91 "return true"];
		numberOfTemporaries: 2;
		buildMethod.

	self
		compileBytecode: 176
		selector: #shortUnconditionalJump
		thenDo: [ :generator | 
			cogit methodObj: method.

			cogit debugStackPointers: (cogit coInterpreter debugStackPointersFor: method).
			
			"Execute the druid's compiled code"
			cogit bytecodePC: 16.
			generator value.
			
			"We will jump over the stop!"
			cogit bytecodePC: 17.
			cogit genReturnFalse.
			cogit Stop.

			cogit bytecodePC: 18.
			cogit patchFixupTargetIfNeeded: (cogit fixupAt: 18) nextOpcodeIndex: cogit opcodeIndex.
			cogit genReturnTrue.
			cogit genUpArrowReturn ].

	self executePrimitiveWithReceiver: (memory integerObjectOf: 17).
	"Did it jump over?"
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiver [

	| object |
	self compileBytecode: 76 selector: #pushReceiverBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.

		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiverVariableBytecode [

	| object |
	self compileBytecode: 0 selector: #pushReceiverVariableBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushReceiverVariableBytecode2 [

	| object |
	self compileBytecode: 1 selector: #pushReceiverVariableBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 1 ofObject: object withValue: 17.
	
	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #tests }
DRProductionBytecodeTest >> testPushTrueObject [

	| object |
	self compileBytecode: 77 selector: #pushConstantTrueBytecode thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value.
	
		"Then return without druid's compiled code"
		cogit ssTop popToReg: ReceiverResultReg.
		cogit genUpArrowReturn ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #tests }
DRProductionBytecodeTest >> testReturnReceiver [

	| object |
	self compileBytecode: 88 selector: #returnReceiver thenDo: [ :generator |
		"Execute the druid's compiled code"
		generator value ].

	object := self newObjectWithSlots: 1.
	memory storePointer: 0 ofObject: object withValue: 17.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: object
]

{ #category : #tests }
DRProductionBytecodeTest >> testReturnTopFromMethod [

	| object |
	self compileBytecode: 92 selector: #returnTopFromMethod thenDo: [ :generator |
		cogit ssPushConstant: 17.

		"Execute the druid's compiled code"
		generator value ].

	object := self newObjectWithSlots: 0.

	self executePrimitiveWithReceiver: object.
	self assert: machineSimulator receiverRegisterValue equals: 17
]
