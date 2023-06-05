"
An infinite register pool simplifies the allocation process as there is no concern about running out of registers.

With an infinite register pool, the register allocator assumes that there is an unlimited number of registers available for allocation. This means that whenever a register allocation request is encountered, the register allocator can always assign a register to the requested variable without any restrictions. 


Internal Representation and Key Implementation Points.

    Instance Variables
	floatingPointRegisters:		<Object>
	integerRegisters:		<Object>
	nextId:		<Object>


    Implementation Points
"
Class {
	#name : #DRInfiniteRegisterPool,
	#superclass : #Object,
	#instVars : [
		'integerRegisters',
		'floatingPointRegisters',
		'nextId'
	],
	#category : #'Druid-LinearScanRegisterAllocation'
}

{ #category : #allocation }
DRInfiniteRegisterPool >> createNewRegister [

	| newRegister |
	newRegister := 'T', nextId asString.
	nextId := nextId + 1.
	^ DRPhysicalGeneralPurposeRegister name: newRegister
]

{ #category : #initialization }
DRInfiniteRegisterPool >> initialize [

	super initialize.
	nextId := 0.
	
	integerRegisters := Stack new.
	floatingPointRegisters := Stack new.
]

{ #category : #allocation }
DRInfiniteRegisterPool >> returnFloatRegister: aRegister [

	floatingPointRegisters push: aRegister
]

{ #category : #allocation }
DRInfiniteRegisterPool >> returnIntegerRegister: aRegister [

	integerRegisters push: aRegister
]

{ #category : #allocation }
DRInfiniteRegisterPool >> takeFloatRegister: aRegister [ 
	
	floatingPointRegisters remove: aRegister
]

{ #category : #allocation }
DRInfiniteRegisterPool >> takeFloatingPointRegisterNotIn: inactiveOverlappingRegisters ifPresent: presentBlock [

	self
		takeRegisterFrom: floatingPointRegisters
		notIn: inactiveOverlappingRegisters
		ifPresent: [ :selectedRegister |
			self takeFloatRegister: selectedRegister.
			^ presentBlock value: selectedRegister ].
	
	^ presentBlock value: self createNewRegister
]

{ #category : #allocation }
DRInfiniteRegisterPool >> takeIntegerRegister: aDRPhysicalRegister [ 
	
	integerRegisters remove: aDRPhysicalRegister
]

{ #category : #allocation }
DRInfiniteRegisterPool >> takeIntegerRegisterNotIn: inactiveOverlappingRegisters ifPresent: presentBlock [

	self
		takeRegisterFrom: integerRegisters
		notIn: inactiveOverlappingRegisters
		ifPresent: [ :selectedRegister |
			self takeIntegerRegister: selectedRegister.
			^ presentBlock value: selectedRegister ].

	^ presentBlock value: self createNewRegister
]

{ #category : #'private - allocation' }
DRInfiniteRegisterPool >> takeRegisterFrom: aCollection notIn: inactiveOverlappingRegisters ifPresent: presentBlock [

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
