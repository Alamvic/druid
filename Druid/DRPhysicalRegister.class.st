Class {
	#name : 'DRPhysicalRegister',
	#superclass : 'DRRegister',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRPhysicalRegister >> isInvariantFrom: loop [

	"Since this is a physical register, it doestn't follows SSA form "

	^ false
]

{ #category : 'cogit-generation' }
DRPhysicalRegister >> isPhysicalRegister [ 

	^ true
]

{ #category : 'register allocation' }
DRPhysicalRegister >> returnToAllocator: aRegisterAllocator [
	
	self subclassResponsibility
]

{ #category : 'cogit-generation' }
DRPhysicalRegister >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'register allocation' }
DRPhysicalRegister >> takeFromAllocator: aRegisterAllocator [ 
	
	self subclassResponsibility
]
