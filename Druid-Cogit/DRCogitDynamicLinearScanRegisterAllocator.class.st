Class {
	#name : 'DRCogitDynamicLinearScanRegisterAllocator',
	#superclass : 'DRCogitLinearScanRegisterAllocator',
	#category : 'Druid-Cogit-RegisterAllocation',
	#package : 'Druid-Cogit',
	#tag : 'RegisterAllocation'
}

{ #category : 'allocation' }
DRCogitDynamicLinearScanRegisterAllocator >> initialize [

	super initialize.
	registerPool := DRInfiniteRegisterPool new
]
