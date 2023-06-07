Class {
	#name : #DRCogitDynamicLinearScanRegisterAllocator,
	#superclass : #DRCogitLinearScanRegisterAllocator,
	#category : #'Druid-Cogit'
}

{ #category : #allocation }
DRCogitDynamicLinearScanRegisterAllocator >> initialize [

	super initialize.
	registerPool := DRInfiniteRegisterPool new
]
