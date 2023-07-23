"
Assumes a limited and fixed number of registers available for allocation. The register allocator has a predefined set of registers to work with, and once all registers are allocated, it needs to make decisions on how to handle additional variables that require registers. This situation is where the register allocator may need to spill registers and store variables in memory to free up registers for new allocations. See `DRLinearScanRegisterAllocator` for details on spilling registers.

Internal Representation and Key Implementation Points.

    Instance Variables
	floatingPointRegisters:		<Object>
	integerRegisters:		<Object>
	spillRegisters:		<Object>


    Implementation Points
"
Class {
	#name : #DRFixedRegisterPool,
	#superclass : #Object,
	#instVars : [
		'integerRegisters',
		'floatingPointRegisters',
		'spillRegisters'
	],
	#category : #'Druid-LinearScanRegisterAllocation'
}

{ #category : #accessing }
DRFixedRegisterPool >> floatingPointRegisters [

	^ floatingPointRegisters
]

{ #category : #accessing }
DRFixedRegisterPool >> floatingPointRegisters: anObject [

	floatingPointRegisters := anObject asOrderedCollection
]

{ #category : #initialization }
DRFixedRegisterPool >> initialize [

	super initialize.
	integerRegisters := #().
	floatingPointRegisters := #().
	
]

{ #category : #accessing }
DRFixedRegisterPool >> integerRegisters [

	^ integerRegisters
]

{ #category : #accessing }
DRFixedRegisterPool >> integerRegisters: anObject [

	integerRegisters := anObject asOrderedCollection
]

{ #category : #'as yet unclassified' }
DRFixedRegisterPool >> peekSpillRegister [

	^ spillRegisters first
]

{ #category : #allocation }
DRFixedRegisterPool >> returnFloatRegister: aRegister [

	floatingPointRegisters addFirst: aRegister
]

{ #category : #allocation }
DRFixedRegisterPool >> returnIntegerRegister: aPhisicalRegister [

	integerRegisters addFirst: aPhisicalRegister
]

{ #category : #'as yet unclassified' }
DRFixedRegisterPool >> returnSpillRegister: aRegister [

	 spillRegisters addFirst: aRegister
]

{ #category : #accessing }
DRFixedRegisterPool >> spillRegisters [

	^ spillRegisters
]

{ #category : #accessing }
DRFixedRegisterPool >> spillRegisters: aCollection [

	spillRegisters := OrderedCollection with: aCollection
]

{ #category : #allocation }
DRFixedRegisterPool >> takeFloatRegister: aRegister [ 
	
	floatingPointRegisters remove: aRegister
]

{ #category : #allocation }
DRFixedRegisterPool >> takeFloatingPointRegisterNotIn: inactiveOverlappingRegisters ifPresent: presentBlock [

	self
		takeRegisterFrom: floatingPointRegisters
		notIn: inactiveOverlappingRegisters
		ifPresent: [ :selectedRegister |
			self takeFloatRegister: selectedRegister.
			^ presentBlock value: selectedRegister ]
]

{ #category : #allocation }
DRFixedRegisterPool >> takeIntegerRegister: aDRPhysicalRegister [ 
	
	integerRegisters remove: aDRPhysicalRegister
]

{ #category : #allocation }
DRFixedRegisterPool >> takeIntegerRegisterNotIn: inactiveOverlappingRegisters ifPresent: presentBlock [

	self
		takeRegisterFrom: integerRegisters
		notIn: inactiveOverlappingRegisters
		ifPresent: [ :selectedRegister |
			self takeIntegerRegister: selectedRegister.
			^ presentBlock value: selectedRegister ]
]

{ #category : #'private - allocation' }
DRFixedRegisterPool >> takeRegisterFrom: aCollection notIn: inactiveOverlappingRegisters ifPresent: presentBlock [

	| currentRegisters |
	currentRegisters := aCollection copy.
	inactiveOverlappingRegisters do: [ :e |
		currentRegisters
			remove: e
			ifAbsent: [ "not care, we just want to ignore that register..." ] ].

	"If we have registers, take one"
	currentRegisters ifNotEmpty: [
		presentBlock value: currentRegisters first ]
]

{ #category : #'as yet unclassified' }
DRFixedRegisterPool >> takeSpillRegister [

	^ spillRegisters removeFirst
]
