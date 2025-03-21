Class {
	#name : 'DRProductionPrimitiveCompilationTest',
	#superclass : 'DRPrimitiveCompilationTest',
	#instVars : [
		'finishCodeAddress',
		'finishEndInstruction'
	],
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'helpers-compiling' }
DRProductionPrimitiveCompilationTest >> buildCompiledBlock: aFullBlockClosure [

	"ATTENTION: This method does not compile the closure block.
	It just put a return compiled in #setUp method (finishCodeAddress)"

	| closure |
	closure := aFullBlockClosure
		           forMemory: memory
		           inMethod: methodBuilder.

	"The compiled block is the second inst var"
	interpreter
		rawHeaderOf: (memory fetchPointer: 1 ofObject: closure)
		put: finishCodeAddress asInteger.
		
	^ closure
]

{ #category : 'running' }
DRProductionPrimitiveCompilationTest >> setUp [

	super setUp.
	self setUpTrampolines.
	cogit computeFullBlockEntryOffsets.
	methodBuilder buildMethod.
	finishCodeAddress := self
		                   generateCogClosure: [ finishEndInstruction := cogit RetN: 0 ]
		                   selector: memory nilObject. 
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAsCharacter [

	self
		compilePrimitive: #primitiveAsCharacter
		ForSendReceiver: (memory integerObjectOf: 1)
		arguments: {  }.

	self runUntilReturn.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory characterObjectOf: 1)
]

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAsFloat [

	self compileDruidPrimitive: #primitiveAsFloat.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42).

	self assert: machineSimulator receiverRegisterValue equals: (memory floatObjectOf: 42.0)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAt [

	| class array |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtByteArrayPadding1 [

	| array |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	array := self new8BitIndexableOfSize: 7.
	memory storeByte: 6 ofObject: array withValue: 42.

	self
		executePrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: 7) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 42)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtByteArrayPadding5 [

	| array |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	array := self new8BitIndexableOfSize: 2.
	memory storeByte: 0 ofObject: array withValue: 42.

	self
		executePrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: 1) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 42)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtByteArrayPadding6 [

	| array index |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	index := 2.
	array := self new8BitIndexableOfSize: 2.
	memory storeByte: index - 1 ofObject: array withValue: 42.

	self
		executePrimitiveWithReceiver: array
		withArguments: { (memory integerObjectOf: index) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 42)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtByteArrayPadding7 [

	| array |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtContext [

	| context |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithNegativeIndex [

	| class array |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithOverflowIndex [

	| class array |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithZeroIndex [

	| class array |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtIsNotCompletePrimitive [

	self timeLimit: (Duration minutes: 2).
	self
		compileDruidPrimitiveGenerator: #primitiveAt
		with: [ :c | "nothing" ].

	self
		compile: [ self assert: cogit gen_primitiveAt equals: 0 ]
		bytecodes: 100
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtManyElements [

	| class array |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtMethod [

	| class method |
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtOnNonArray [

	| ephemeron |
	self skip. "FIXME"

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	self createEphemeronClass.
	ephemeron := self newEphemeronObject.

	self
		executeUntilStopPrimitiveWithReceiver: ephemeron
		withArguments: { (memory integerObjectOf: 1) }
]

{ #category : 'tests-object-modify' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtPut [

	| class array |
	self timeLimit: (Duration minutes: 5).
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

{ #category : 'tests-object-modify' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtPutImmutable [

	| class array |
	self timeLimit: (Duration minutes: 5).
	cogit ceStoreCheckTrampoline: fakeTrampoline.

	self compileDruidPrimitive: #primitiveAtPut.

	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	memory setIsImmutableOf: array to: true.


	self executeUntilStopPrimitiveWithReceiver: array withArguments: {
			(memory integerObjectOf: 1).
			(memory integerObjectOf: 42) }.

	self assert: (memory fetchPointer: 0 ofObject: array) equals: memory nilObject
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveClass [

	| class |
	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory nonIndexablePointerFormat.
	obj := memory instantiateClass: class.


	self
		compilePrimitive: #primitiveClass
		ForSendReceiver: obj
		arguments: {  }.
	
	self runUntilReturn.

	self assert: machineSimulator receiverRegisterValue equals: class
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveClassImmediate [

	| int |
	int := memory integerObjectOf: 42.

	self
		compilePrimitive: #primitiveClass
		ForSendReceiver: int
		arguments: {  }.
	
	self runUntilReturn.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory fetchClassOf: int)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveClassWithArg [

	| class |
	class := self
		         newClassInOldSpaceWithSlots: 0
		         instSpec: memory nonIndexablePointerFormat.
	obj := memory instantiateClass: class.

	self
		compilePrimitive: #primitiveClass
		ForSendReceiver: memory nilObject
		arguments: { obj }.
	
	self runUntilReturn.

	self assert: machineSimulator receiverRegisterValue equals: class
]

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatAdd [

	self compileDruidPrimitive: #primitiveFloatAdd.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory floatObjectOf: 3.0)
]

{ #category : 'tests-floats' }
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

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatEqual [

	self compileDruidPrimitive: #primitiveFloatEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 1.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatEqualFalse [

	self compileDruidPrimitive: #primitiveFloatEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory falseObject
]

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatMultiply [

	self compileDruidPrimitive: #primitiveFloatMultiply.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 3.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory floatObjectOf: 6.0)
]

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatNotEqual [

	self compileDruidPrimitive: #primitiveFloatNotEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory trueObject
]

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatNotEqualFalse [

	self compileDruidPrimitive: #primitiveFloatNotEqual.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 1.0)
		withArguments: { (memory floatObjectOf: 1.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: memory falseObject
]

{ #category : 'tests-floats' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveFloatSubtract [

	self compileDruidPrimitive: #primitiveFloatSubtract.

	self
		executePrimitiveWithReceiver: (memory floatObjectOf: 3.0)
		withArguments: { (memory floatObjectOf: 2.0) }.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory floatObjectOf: 1.0)
]

{ #category : 'tests-hash' }
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

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveImmediateAsInteger [

	self
		compilePrimitive: #primitiveImmediateAsInteger
		ForSendReceiver: (memory integerObjectOf: 1)
		arguments: {  }.

	self runUntilReturn.

	self
		assert: machineSimulator receiverRegisterValue
		equals: (memory integerObjectOf: 1)
]

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveNewFailingForLongSlotsObjects [

	| class |
	self compileDruidPrimitive: #primitiveNew.

	class := class := self
		                  newClassInOldSpaceWithSlots: 256
		                  instSpec: memory nonIndexablePointerFormat.
	memory ensureBehaviorHash: class.

	self executeUntilStopPrimitiveWithReceiver: class
]

{ #category : 'tests-object-instantiation' }
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

{ #category : 'tests-object-instantiation' }
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

{ #category : 'tests-object-instantiation' }
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

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeArray [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 7.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeArrayExtended [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 256.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeAsParameter [

	| class array size |
	self skip. "Not implemented - failing with multiargs"

	self timeLimit: (Duration minutes: 2).

	size := 7.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: size.
	
	self
		compilePrimitive: #primitiveSize
		ForSendReceiver: memory nilObject
		arguments: { array }.

	self runUntilReturn.

	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeByteArray [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 256.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory firstByteFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeByteArrayPadding0 [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 8.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory firstByteFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeByteArrayPadding1 [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 7.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory firstByteFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeByteArrayPadding7 [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 1.
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory firstByteFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeIndexableObject [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 7.
	class := self newClassInOldSpaceWithSlots: 2 instSpec: memory indexablePointersFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeNonIndexable [

	| class |
	self compileDruidPrimitive: #primitiveSize.

	class := self newClassInOldSpaceWithSlots: 2 instSpec: memory nonIndexablePointerFormat.
	obj := memory instantiateClass: class.

	self executeUntilStopPrimitiveWithReceiver: obj
]

{ #category : 'tests-object-access' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveSizeWeakArray [

	| class array size |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveSize.

	size := 7.
	class := self newClassInOldSpaceWithSlots: 2 instSpec: memory weakArrayFormat.
	array := memory instantiateClass: class indexableSize: size.

	self executePrimitiveWithReceiver: array.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: size)
]

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesArray [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesByteArray [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesInteger16Array [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesInteger32Array [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInitializesInteger64Array [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesArray [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesByteArray [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesInteger16Array [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesInteger32Array [

	| class |
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

{ #category : 'tests-object-instantiation' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveWithArgNewInstantiatesInteger64Array [

	| class |
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

{ #category : 'tests-hash' }
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

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveAdd [

	cogit receiverTags: memory smallIntegerTag.

	self compileDruidPrimitive: #primitiveAdd.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 52)
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveBitAnd [

	cogit receiverTags: memory smallIntegerTag.

	self compileDruidPrimitive: #primitiveBitAnd.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 11 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: (42 bitAnd: 11))
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValue [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	closure := self buildCompiledBlock: [ 42 ].

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: {  }.
	
	"Compiled block executed"
	self runUntil: finishEndInstruction address
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueOnNonClosureMethod [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	closure := self buildCompiledBlock: [ 42 ].

	"Set nil as compiled block"
	memory storePointer: 1 ofObject: closure withValue: memory nilObject.

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: {  }.

	self runUntilStop
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueOnNonCompiledBlock [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	closure := [ 42 ] forMemory: memory inMethod: methodBuilder.

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: {  }.

	self runUntilStop
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueOnWrongArguments [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	closure := self buildCompiledBlock: [ :x | 42 + x ].

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: {  }.

	self runUntilStop
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValuePushReceiver [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure initStack endStack pushedSize |
	closure := self buildCompiledBlock: [ 42 ].

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: { }.

	initStack := machineSimulator smalltalkStackPointerRegisterValue.

	self runUntil: finishEndInstruction address.

	endStack := machineSimulator smalltalkStackPointerRegisterValue.

	pushedSize := (initStack - endStack) // 8.

	"this primitive does not push anything to the stack and jumps to the block"
	self assert: pushedSize equals: 0

]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValuePushReceiverAndArg [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure initStack endStack pushedSize |
	closure := self buildCompiledBlock: [ :x | 42 + x ].

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: { (memory integerObjectOf: 3) }.

	initStack := machineSimulator smalltalkStackPointerRegisterValue.

	self runUntil: finishEndInstruction address.

	endStack := machineSimulator smalltalkStackPointerRegisterValue.

	pushedSize := (initStack - endStack) // 8.

	"this primitive does not push anything to the stack and jumps to the block"
	self assert: pushedSize equals: 0
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueWithArg [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	closure := self buildCompiledBlock: [ :x | 42 + x ].

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: { (memory integerObjectOf: 3) }.

	"Compiled block executed"
	self runUntil: finishEndInstruction address
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueWithArgs [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	closure := self buildCompiledBlock: [ :x :y | 42 + x + y ].

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: {
				(memory integerObjectOf: 3).
				(memory integerObjectOf: 7) }.

	"Compiled block executed"
	self runUntil: finishEndInstruction address
]

{ #category : 'tests-arithmetic-smallIntegers' }
DRProductionPrimitiveCompilationTest >> testPrimitiveValueWithTooManyArgs [

	<compilerOptions: #( - #optionConstantBlockClosure )>
	| closure |
	closure := self buildCompiledBlock: [ :x :y :z | 42 + x + y + z ].

	self
		compilePrimitive: #primitiveFullClosureValue
		ForSendReceiver: closure
		arguments: {
				(memory integerObjectOf: 3).
				(memory integerObjectOf: 5).
				(memory integerObjectOf: 7) }.

	self runUntilStop
]
