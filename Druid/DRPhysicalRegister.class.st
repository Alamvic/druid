Class {
	#name : #DRPhysicalRegister,
	#superclass : #DRValue,
	#instVars : [
		'name'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRPhysicalRegister class >> name: aString [ 
	
	^ self new
		name: aString;
		yourself
]

{ #category : #comparing }
DRPhysicalRegister >> = anotherRegister [

	self species = anotherRegister species
		ifFalse: [ ^ false ].
	^ name = anotherRegister name
]

{ #category : #memory }
DRPhysicalRegister >> asMemoryAddress [
	
	^ DRRegisterMemoryAddress register: self
]

{ #category : #comparing }
DRPhysicalRegister >> hash [

	^ name hash
]

{ #category : #accessing }
DRPhysicalRegister >> id [
	
	^ name
]

{ #category : #'cogit-generation' }
DRPhysicalRegister >> isPhysicalRegister [ 

	^ true
]

{ #category : #testing }
DRPhysicalRegister >> isRegister [
	
	^ true
]

{ #category : #accessing }
DRPhysicalRegister >> name [
	^ name
]

{ #category : #accessing }
DRPhysicalRegister >> name: aString [ 
	name := aString
]

{ #category : #printing }
DRPhysicalRegister >> printOn: aStream [

	aStream
		nextPutAll: 'DRRegister(';
		print: name;
		nextPut: $)
]

{ #category : #'cogit-generation' }
DRPhysicalRegister >> rtlOperandQualifier [
	
	^ 'R:'
]

{ #category : #'cogit-generation' }
DRPhysicalRegister >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #types }
DRPhysicalRegister >> type [
	
	^ DRIntegerType new
]
