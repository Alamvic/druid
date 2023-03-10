Class {
	#name : #DRProductionPrimitiveCompilationTest,
	#superclass : #DRPrimitiveCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #'tests-real primitives' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAt [

	| class array |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.
	memory storePointer: 0 ofObject: array withValue: 42.

	self 	executePrimitiveWithReceiver: array 
			withArguments: { memory integerObjectOf: 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
	
]

{ #category : #'tests-real primitives' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithNegativeIndex [

	| class array |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	self executeUntilStopPrimitiveWithReceiver: array withArguments: { memory integerObjectOf: -2 }
]

{ #category : #'tests-real primitives' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithOverflowIndex [

	| class array |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	self executeUntilStopPrimitiveWithReceiver: array withArguments: { memory integerObjectOf: 2 }
]

{ #category : #'tests-real primitives' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtFaillingWithZeroIndex [

	| class array |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.

	self executeUntilStopPrimitiveWithReceiver: array withArguments: { memory integerObjectOf: 0 }
]

{ #category : #'tests-real primitives' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveAtManyElements [

	| class array |
	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveAt.

	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 2.
	memory storePointer: 0 ofObject: array withValue: 40.
	memory storePointer: 1 ofObject: array withValue: 41.

	self 	executePrimitiveWithReceiver: array 
			withArguments: { memory integerObjectOf: 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 41
	
]

{ #category : #'tests-real primitives' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveNewInitializesInstance [

	| class |
	self timeLimit: (Duration minutes: 2).
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

{ #category : #'tests-real primitives' }
DRProductionPrimitiveCompilationTest >> testCompilePrimitiveNewInstantiatesClass [

	| class |
	self timeLimit: (Duration minutes: 2).
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

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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
		withArguments: { memory integerObjectOf: 3 }.

	self
		assert:
		(memory fetchClassOf: machineSimulator receiverRegisterValue)
		equals: class
]

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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

{ #category : #'tests-real primitives' }
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
