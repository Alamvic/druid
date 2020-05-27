Class {
	#name : #DRLogicalRegister,
	#superclass : #DRRegister,
	#category : #'Druid-Instructions'
}

{ #category : #'instance creation' }
DRLogicalRegister class >> basePointer [

	^ self new
		name: #BasePointer;
		yourself
]

{ #category : #testing }
DRLogicalRegister >> isPhysical [
	
	^ false
]

{ #category : #accessing }
DRLogicalRegister >> registers [
	^ { self }
]

{ #category : #'register allocation' }
DRLogicalRegister >> updateLogicalRegisters: mappings [

	^ mappings at: self
]
