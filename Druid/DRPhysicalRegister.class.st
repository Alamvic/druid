Class {
	#name : #DRPhysicalRegister,
	#superclass : #DRRegister,
	#category : #'Druid-Instructions'
}

{ #category : #'instance creation' }
DRPhysicalRegister class >> basePointer [

	^ self named: #BasePointer
]

{ #category : #tesing }
DRPhysicalRegister >> isPhysical [
	
	^ true
]

{ #category : #accessing }
DRPhysicalRegister >> registers [
	
	^ { self }
]

{ #category : #'register allocation' }
DRPhysicalRegister >> updateLogicalRegisters: mappings [ 
	
	^ self
]
