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

{ #category : #comparing }
DRPhysicalRegister >> hash [

	^ name hash
]

{ #category : #accessing }
DRPhysicalRegister >> id [
	
	^ name
]

{ #category : #testing }
DRPhysicalRegister >> isInvariantFrom: loopLatchBasicBlock [

	"Since this is a physical register, it doestn't follows SSA form "

	^ false
]

{ #category : #'cogit-generation' }
DRPhysicalRegister >> isPhysicalRegister [ 

	^ true
]

{ #category : #testing }
DRPhysicalRegister >> isRegister [
	
	^ true
]

{ #category : #testing }
DRPhysicalRegister >> isValue [

	^ false
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

{ #category : #'register allocation' }
DRPhysicalRegister >> returnToAllocator: aRegisterAllocator [
	
	self subclassResponsibility
]

{ #category : #'cogit-generation' }
DRPhysicalRegister >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #'register allocation' }
DRPhysicalRegister >> takeFromAllocator: aRegisterAllocator [ 
	
	self subclassResponsibility
]
