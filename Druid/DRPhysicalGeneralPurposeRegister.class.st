Class {
	#name : #DRPhysicalGeneralPurposeRegister,
	#superclass : #DRPhysicalRegister,
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRPhysicalGeneralPurposeRegister class >> name: aString [ 
	
	^ self new
		name: aString;
		yourself
]

{ #category : #visiting }
DRPhysicalGeneralPurposeRegister >> acceptVisitor: aVisitor [

	^ aVisitor visitPhysicalRegister: self
]

{ #category : #memory }
DRPhysicalGeneralPurposeRegister >> asMemoryAddress [
	
	^ DRRegisterMemoryAddress register: self
]

{ #category : #'cogit-generation' }
DRPhysicalGeneralPurposeRegister >> rtlOperandQualifier [
	
	^ 'R:'
]

{ #category : #types }
DRPhysicalGeneralPurposeRegister >> type [
	
	^ DRIntegerType new
]
