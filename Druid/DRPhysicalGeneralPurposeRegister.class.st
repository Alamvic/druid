Class {
	#name : #DRPhysicalGeneralPurposeRegister,
	#superclass : #DRPhysicalRegister,
	#category : #'Druid-IR'
}

{ #category : #factory }
DRPhysicalGeneralPurposeRegister class >> classReg [

	^ self name: 'ClassReg'
]

{ #category : #factory }
DRPhysicalGeneralPurposeRegister class >> receiverResultReg [

	^ self name: 'ReceiverResultReg'
]

{ #category : #factory }
DRPhysicalGeneralPurposeRegister class >> sendNumArgsReg [

	^ self name: 'SendNumArgsReg'
]

{ #category : #visiting }
DRPhysicalGeneralPurposeRegister >> acceptVisitor: aVisitor [

	^ aVisitor visitPhysicalRegister: self
]

{ #category : #memory }
DRPhysicalGeneralPurposeRegister >> asMemoryAddress [
	
	^ DRRegisterMemoryAddress register: self
]

{ #category : #printing }
DRPhysicalGeneralPurposeRegister >> printResult [

	^ name
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

	^ DRSignedIntegerType size: 8 "Word size"
]
