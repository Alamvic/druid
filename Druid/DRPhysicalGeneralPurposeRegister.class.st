Class {
	#name : #DRPhysicalGeneralPurposeRegister,
	#superclass : #DRPhysicalRegister,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRPhysicalGeneralPurposeRegister >> acceptVisitor: aVisitor [

	^ aVisitor visitPhysicalRegister: self
]

{ #category : #memory }
DRPhysicalGeneralPurposeRegister >> asMemoryAddress [
	
	^ DRRegisterMemoryAddress register: self
]

{ #category : #'register allocation' }
DRPhysicalGeneralPurposeRegister >> returnToAllocator: aRegisterAllocator [
	
	aRegisterAllocator returnIntegerRegister: self
]

{ #category : #'cogit-generation' }
DRPhysicalGeneralPurposeRegister >> rtlOperandQualifier [
	
	^ 'R:'
]

{ #category : #'register allocation' }
DRPhysicalGeneralPurposeRegister >> takeFromAllocator: aRegisterAllocator [ 
	
	aRegisterAllocator takeIntegerRegister: self
]

{ #category : #types }
DRPhysicalGeneralPurposeRegister >> type [
	
	^ DRIntegerType new
]
