Class {
	#name : #DRProductionPrimitiveCompilationTest,
	#superclass : #DRPrimitiveCompilationTest,
	#instVars : [
		'finishCodeAddress',
		'finishEndInstruction'
	],
	#category : #'Druid-Tests'
}

{ #category : #running }
DRProductionPrimitiveCompilationTest >> setUp [

	super setUp.
	self setUpTrampolines.
	cogit computeFullBlockEntryOffsets.
	methodBuilder buildMethod.
	finishCodeAddress := self
		                   generateCogClosure: [ finishEndInstruction := cogit RetN: 0 ]
		                   selector: memory nilObject. 
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAsCharacter [

	self compileDruidPrimitive: #primitiveAsCharacter.

	self 	executePrimitiveWithReceiver: (memory integerObjectOf: 1). 
				
	self assert: machineSimulator receiverRegisterValue equals: (memory characterObjectOf: 1)
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAsFloat [

	self compileDruidPrimitive: #primitiveAsFloat.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42).

	self assert: machineSimulator receiverRegisterValue equals: (memory floatObjectOf: 42.0)
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAt [

	| class array |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.
	memory storePointer: 0 ofObject: array withValue: 42.

	self
		executePrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: 1) }.

	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtByteArray [

	| array |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	array := self new8BitIndexableOfSize: 1.
	memory storeByte: 0 ofObject: array withValue: 42.

	self
		executePrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: 1) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 42)
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtContext [

	| context |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	context := memory
		           allocateSlots: SmallContextSlots
		           format: memory indexablePointersFormat
		           classIndex: ClassMethodContextCompactIndex.

	self
		executeUntilStopPrimitiveWithReceiver: context
		withArguments: { (memory integerObjectOf: 1) }
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithNegativeIndex [

	| class array |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	self
		executeUntilStopPrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: -2) }
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithOverflowIndex [

	| class array |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	self
		executeUntilStopPrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: 2) }
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithZeroIndex [

	| class array |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	self
		executeUntilStopPrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: 0) }
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtIsNotCompletePrimitive [

	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self
		compileDruidPrimitiveGenerator: #primitiveAt
		with: [ :c | "nothing" ].

	self
		compile: [ self assert: cogit gen_primitiveAt equals: 0 ]
		bytecodes: 100
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtManyElements [

	| class array |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 2.
	memory storePointer: 0 ofObject: array withValue: 40.
	memory storePointer: 1 ofObject: array withValue: 41.

	self
		executePrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: 2) }.

	self assert: machineSimulator receiverRegisterValue equals: 41
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtMethod [

	| class method |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory firstCompiledMethodFormat.
	method := memory
		          instantiateCompiledMethodClass: class
		          indexableSize: 10.

	self
		executeUntilStopPrimitiveWithReceiver: method
		withArguments: { (memory integerObjectOf: 1) }
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtOnNonArray [

	| ephemeron |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	self createEphemeronClass.
	ephemeron := self newEphemeronObject.

	self
		executeUntilStopPrimitiveWithReceiver: ephemeron
		withArguments: { (memory integerObjectOf: 1) }
]

{ #category : #'tests-object-modify' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtPut [

	| class array |
	self timeLimit: (Duration minutes: 2).
	cogit ceStoreCheckTrampoline: fakeTrampoline.

	self compileDruidPrimitive: #primitiveAtPut.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	self executePrimitiveWithReceiver: array withArguments: {
			(memory integerObjectOf: 1).
			(memory integerObjectOf: 42) }.

	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 42).

	self
		assert: (memory fetchPointer: 0 ofObject: array)
		equals: (memory integerObjectOf: 42)
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveClass [

	| class |
	self compileDruidPrimitive: #primitiveClass.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory nonIndexablePointerFormat.
	obj := memory instantiateClass: class.

	self executePrimitiveWithReceiver: obj.

	self assert: machineSimulator receiverRegisterValue equals: class
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveClassFaillingWithArg [

	| class |
	self compileDruidPrimitive: #primitiveClass numArgs: 1.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory nonIndexablePointerFormat.
	obj := memory instantiateClass: class.

	self
		executeUntilStopPrimitiveWithReceiver: memory nilObject
		withArguments: { obj }
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveClassImmediate [

	| int |
	self compileDruidPrimitive: #primitiveClass.

	int := memory integerObjectOf: 42.

	self executePrimitiveWithReceiver: int.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory fetchClassOf: int)
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatAdd [

	self compileDruidPrimitive: #primitiveFloatAdd.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory floatObjectOf: 3.0)
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatDivide [

	self skip. "TOFIX: self CmpCq: 0.0 Rd: DPFPReg1."

	self compileDruidPrimitive: #primitiveFloatDivide.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 3.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory floatObjectOf: 1.5)
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatEqual [

	self compileDruidPrimitive: #primitiveFloatEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 1.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatEqualFalse [

	self compileDruidPrimitive: #primitiveFloatEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory falseObject
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatMultiply [

	self compileDruidPrimitive: #primitiveFloatMultiply.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 3.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory floatObjectOf: 6.0)
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatNotEqual [

	self compileDruidPrimitive: #primitiveFloatNotEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatNotEqualFalse [

	self compileDruidPrimitive: #primitiveFloatNotEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 1.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory falseObject
]

{ #category : #'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatSubtract [

	self compileDruidPrimitive: #primitiveFloatSubtract.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 3.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory floatObjectOf: 1.0)
]

{ #category : #'tests-hash' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveIdentityHash [

	| hash |
	cogit ceNewHashTrampoline: fakeTrampoline.

	self compileDruidPrimitive: #primitiveIdentityHash.

	obj := self newObjectWithSlots: 0.

	self executePrimitiveWithReceiver: obj.

	"Fake trampoline do nothing -> same object is returned as hash and is used as an integer."
	hash := memory integerObjectOf: obj.

	self assert: machineSimulator receiverRegisterValue equals: hash
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveImmediateAsInteger [

	self compileDruidPrimitive: #primitiveImmediateAsInteger.

	self 	executePrimitiveWithReceiver: (memory characterObjectOf: 1). 
				
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 1)
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveNewFailingForLongSlotsObjects [

	| class |
	self compileDruidPrimitive: #primitiveNew.

	class := class := self
		                  newClassInOldSpaceWithSlots: 256
		                  instSpec: memory nonIndexablePointerFormat.
	memory ensureBehaviorHash: class.

	self executeUntilStopPrimitiveWithReceiver: class
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveNewInitializesInstance [

	| class |
	self compileDruidPrimitive: #primitiveNew.

	class := self
		         newClassInOldSpaceWithSlots: 4
		         instSpec: memory nonIndexablePointerFormat.
	memory ensureBehaviorHash: class.
	self executePrimitiveWithReceiver: class.

	self assert: (memory numSlotsOf: machineSimulator receiverRegisterValue) equals: 4.
	0 to: 3 do: [ :e |
		self
			assert: (memory fetchPointer: e ofObject: machineSimulator receiverRegisterValue)
			equals: memory nilObject
	]
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveNewInstantiatesClass [

	| class |
	self compileDruidPrimitive: #primitiveNew.

	class := self
		         newClassInOldSpaceWithSlots: 4
		         instSpec: memory nonIndexablePointerFormat.
	memory ensureBehaviorHash: class.
	self executePrimitiveWithReceiver: class.

	self
		assert:
		(memory fetchClassOf: machineSimulator receiverRegisterValue)
		equals: class
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveNewMoveStartFreeSpace [

	| class freeStart newObjectSize |
	self compileDruidPrimitive: #primitiveNew.

	class := self
		         newClassInOldSpaceWithSlots: 4
		         instSpec: memory nonIndexablePointerFormat.
	memory ensureBehaviorHash: class.
	freeStart := memory freeStart.

	self executePrimitiveWithReceiver: class.

	newObjectSize := (memory byteSizeOf: machineSimulator receiverRegisterValue) + memory baseHeaderSize.
	self assert: memory freeStart equals: freeStart + newObjectSize
]

{ #category : #'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSize [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 7.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesArray [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory arrayFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.
	
	self assert: (memory stSizeOf: machineSimulator receiverRegisterValue) equals: 3.
	0 to: 2 do: [ :e |
		self
			assert: (memory fetchPointer: e ofObject: machineSimulator receiverRegisterValue)
			equals: memory nilObject
	]
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesByteArray [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory firstByteFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self assert: (memory stSizeOf: machineSimulator receiverRegisterValue) equals: 3.
	0 to: 2 do: [ :e |
		self
			assert: (memory fetchByte: e ofObject: machineSimulator receiverRegisterValue)
			equals: 0
	]
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesInteger16Array [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory firstShortFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self assert: (memory stSizeOf: machineSimulator receiverRegisterValue) equals: 3.
	0 to: 2 do: [ :e |
		self
			assert: (memory fetchShort16: e ofObject: machineSimulator receiverRegisterValue)
			equals: 0
	]
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesInteger32Array [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory firstLongFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self assert: (memory stSizeOf: machineSimulator receiverRegisterValue) equals: 3.
	0 to: 2 do: [ :e |
		self
			assert: (memory fetchLong32: e ofObject: machineSimulator receiverRegisterValue)
			equals: 0
	]
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesInteger64Array [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory sixtyFourBitIndexableFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self assert: (memory stSizeOf: machineSimulator receiverRegisterValue) equals: 3.
	0 to: 2 do: [ :e |
		self
			assert: (memory fetchLong64: e ofObject: machineSimulator receiverRegisterValue)
			equals: 0
	]
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesArray [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 4
		         instSpec: memory arrayFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { (memory integerObjectOf: 3) }.

	self
		assert:
		(memory fetchClassOf: machineSimulator receiverRegisterValue)
		equals: class
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesByteArray [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory firstByteFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self
		assert:
		(memory fetchClassOf: machineSimulator receiverRegisterValue)
		equals: class
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesInteger16Array [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory firstShortFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self
		assert:
		(memory fetchClassOf: machineSimulator receiverRegisterValue)
		equals: class
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesInteger32Array [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory firstLongFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self
		assert:
		(memory fetchClassOf: machineSimulator receiverRegisterValue)
		equals: class
]

{ #category : #'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesInteger64Array [

	| class |
	(cogit isKindOf: SimpleDruidTestRTLCompiler) ifTrue: [ ^ self skip ].
	
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNewWithArg.

	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory sixtyFourBitIndexableFormat.
	memory ensureBehaviorHash: class.
	self
		executePrimitiveWithReceiver: class
		withArguments: { memory integerObjectOf: 3 }.

	self
		assert:
		(memory fetchClassOf: machineSimulator receiverRegisterValue)
		equals: class
]

{ #category : #'tests-hash' }
DRProductionPrimitiveCompilationTest >> testNewHashTrampoline [

	| hash newHashTrampoline frame |
	newHashTrampoline := cogit
		                     genNewHashTrampoline: false
		                     called: 'ceInlineNewHash'.

	cogInitialAddress := self
		                     compile: [
			                     cogit MoveR: ReceiverResultReg R: TempReg.
			                     cogit backEnd saveAndRestoreLinkRegAround: [
				                     cogit CallRT: newHashTrampoline ].
			                     cogit MoveR: TempReg R: ReceiverResultReg.
			                     cogit genPrimReturn ]
		                     bytecodes: 100.

	"Prepare the frame to be able to jump to the interpreter"
	frame := stackBuilder addNewFrame.
	stackBuilder buildStack.
	machineSimulator fp: interpreter framePointer.
	machineSimulator smalltalkStackPointerRegisterValue:
		interpreter stackPointer.

	obj := self newObjectWithSlots: 0.

	self executePrimitiveWithReceiver: obj.

	hash := memory rawHashBitsOf: obj.

	self deny: hash equals: 0. "Hash was computed"

	self assert: machineSimulator receiverRegisterValue equals: hash
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveAdd [

	cogit receiverTags: memory smallIntegerTag.

	self compileDruidPrimitive: #primitiveAdd.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 52)
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValue [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	self compileDruidPrimitive: #primitiveFullClosureValue.

	closure := [ 42 ] forMemory: memory inMethod: methodBuilder.

	interpreter
		rawHeaderOf: (memory fetchPointer: 1 ofObject: closure) "The compiled block is the second inst var"
		put: finishCodeAddress asInteger.

	self prepareStackForSendReceiver: closure arguments: {  }.
	self runUntil: finishEndInstruction address
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueOnNonClosureMethod [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	self compileDruidPrimitive: #primitiveFullClosureValue.

	closure := [ 42 ] forMemory: memory inMethod: methodBuilder.
	
	memory
		storePointer: 1 "The compiled block is the second inst var"
		ofObject: closure
		withValue: memory nilObject.

	self executeUntilStopPrimitiveWithReceiver: closure
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueOnNonCompiledBlock [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	self compileDruidPrimitive: #primitiveFullClosureValue.

	closure := [ 42 ] forMemory: memory inMethod: methodBuilder.

	self executeUntilStopPrimitiveWithReceiver: closure
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueOnWrongArguments [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	self compileDruidPrimitive: #primitiveFullClosureValue.

	closure := [ :x | 42 ] forMemory: memory inMethod: methodBuilder.

	self executeUntilStopPrimitiveWithReceiver: closure
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValuePushReceiver [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure initStack endStack pushedSize |
	self compileDruidPrimitive: #primitiveFullClosureValue.

	closure := [ 42 ] forMemory: memory inMethod: methodBuilder.

	interpreter
		rawHeaderOf: (memory fetchPointer: 1 ofObject: closure) "The compiled block is the second inst var"
		put: finishCodeAddress asInteger.

	initStack := machineSimulator smalltalkStackPointerRegisterValue.

	self prepareStackForSendReceiver: closure arguments: {  }.
	self runUntil: finishEndInstruction address.

	endStack := machineSimulator smalltalkStackPointerRegisterValue.

	pushedSize := (initStack - endStack) // 8.

	self assert: pushedSize equals: 1 "prepare the Stack in the test" + 1 "this primitive must push the values"

]
