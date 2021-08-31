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
DRPhysicalRegister >> sccpEvaluateFor: sccp [

	^ self value
]
