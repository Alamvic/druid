Class {
	#name : #DRCogitDynamicLinearScanRegisterAllocator,
	#superclass : #DRCogitLinearScanRegisterAllocator,
	#instVars : [
		'nextRegister'
	],
	#category : #'Druid-Cogit'
}

{ #category : #allocation }
DRCogitDynamicLinearScanRegisterAllocator >> allocateRegisterForInterval: anInterval ifPresent: aBlock [

	"Always allocate a new register"
	| allocatedRegister |
	allocatedRegister := nextRegister.
	nextRegister := nextRegister + 1.
	^ aBlock value: (DRPhysicalGeneralPurposeRegister name: 't', allocatedRegister asString)
]

{ #category : #allocation }
DRCogitDynamicLinearScanRegisterAllocator >> initialize [

	super initialize.
	nextRegister := 0
]
