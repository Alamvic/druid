Class {
	#name : 'DRCogitDynamicLinearScanRegisterAllocator',
	#superclass : 'DRCogitLinearScanRegisterAllocator',
	#category : 'Druid-Cogit',
	#package : 'Druid',
	#tag : 'Cogit'
}

{ #category : 'allocation' }
DRCogitDynamicLinearScanRegisterAllocator >> initialize [

	super initialize.
	registerPool := DRInfiniteRegisterPool new
]
