Class {
	#name : 'DRPhysicalFloatingPointRegister',
	#superclass : 'DRPhysicalRegister',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRPhysicalFloatingPointRegister >> acceptVisitor: aVisitor [

	^ aVisitor visitPhysicalRegister: self
]

{ #category : 'register allocation' }
DRPhysicalFloatingPointRegister >> returnToAllocator: anAllocator [ 

	anAllocator returnFloatRegister: self
]

{ #category : 'cogit-generation' }
DRPhysicalFloatingPointRegister >> rtlOperandQualifier [
	
	^ 'Rd:'
]

{ #category : 'register allocation' }
DRPhysicalFloatingPointRegister >> takeFromAllocator: aRegisterAllocator [
	
	aRegisterAllocator takeFloatRegister: self
]

{ #category : 'types' }
DRPhysicalFloatingPointRegister >> typeWithAlreadySeen: aCollection [
	
	^ DRFloatType new
]
