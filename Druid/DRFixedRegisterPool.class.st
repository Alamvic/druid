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

{ #category : #allocation }
DRFixedRegisterPool >> returnFloatRegister: aRegister [

	floatingPointRegisters addFirst: aRegister
]

{ #category : #allocation }
DRFixedRegisterPool >> returnIntegerRegister: aPhisicalRegister [

	integerRegisters addFirst: aPhisicalRegister
]

{ #category : #accessing }
DRFixedRegisterPool >> spillRegisters [

	^ spillRegisters
]

{ #category : #accessing }
DRFixedRegisterPool >> spillRegisters: anObject [

	spillRegisters := anObject
]

{ #category : #allocation }
DRFixedRegisterPool >> takeFloatRegister: aRegister [ 
	
	floatingPointRegisters remove: aRegister
]

{ #category : #allocation }
DRFixedRegisterPool >> takeIntegerRegister: aDRPhysicalRegister [ 
	
	integerRegisters remove: aDRPhysicalRegister
]
