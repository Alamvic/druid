Class {
	#name : #DRRegister,
	#superclass : #DRValue,
	#instVars : [
		'name'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRRegister class >> name: aString [ 
	
	^ self new
		name: aString;
		yourself
]

{ #category : #comparing }
DRRegister >> = anotherRegister [

	self == anotherRegister ifTrue: [ ^ true ].
	self species = anotherRegister species
		ifFalse: [ ^ false ].
	^ name = anotherRegister name
]

{ #category : #comparing }
DRRegister >> hash [

	^ name hash
]

{ #category : #accessing }
DRRegister >> id [
	
	^ name
]

{ #category : #testing }
DRRegister >> isRegister [
	
	^ true
]

{ #category : #testing }
DRRegister >> isStagedRegister [
	
	^ false
]

{ #category : #testing }
DRRegister >> isValue [

	^ false
]

{ #category : #accessing }
DRRegister >> name [
	^ name asString
]

{ #category : #accessing }
DRRegister >> name: aString [ 
	name := aString
]

{ #category : #printing }
DRRegister >> prettyPrint [

	^ self name asString
]

{ #category : #printing }
DRRegister >> printOn: aStream [

	aStream
		nextPutAll: self class name , '(';
		print: name;
		nextPut: $)
]

{ #category : #cogit }
DRRegister >> rtlPushOperandQualifierForInstruction: anInstruction [

	^ 'Register:'
]
