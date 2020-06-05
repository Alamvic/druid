Class {
	#name : #DRInterpreterContext,
	#superclass : #Object,
	#instVars : [
		'parent',
		'method',
		'receiver',
		'arguments',
		'block',
		'temporaries'
	],
	#category : #'Druid-Interpreter'
}

{ #category : #accessing }
DRInterpreterContext >> argumentNamed: aString [ 
	
	| index |
	index := (method argumentNames indexOf: aString).
	
	^ index ~= 0 
		ifTrue: [ arguments at: index ]
		ifFalse: [ block home argumentNamed: aString ]
	
]

{ #category : #accessing }
DRInterpreterContext >> arguments [
	^ arguments
]

{ #category : #accessing }
DRInterpreterContext >> arguments: anObject [
	arguments := anObject
]

{ #category : #accessing }
DRInterpreterContext >> block: aDRBlock [	

	block := aDRBlock 
]

{ #category : #accessing }
DRInterpreterContext >> initialize [

	super initialize.
	temporaries := Dictionary new
]

{ #category : #accessing }
DRInterpreterContext >> method [
	^ method
]

{ #category : #accessing }
DRInterpreterContext >> method: anObject [
	method := anObject
]

{ #category : #accessing }
DRInterpreterContext >> parent [
	^ parent
]

{ #category : #accessing }
DRInterpreterContext >> parent: anObject [
	parent := anObject
]

{ #category : #printing }
DRInterpreterContext >> printOn: aStream [

	super printOn: aStream.
	aStream space; print: method
]

{ #category : #accessing }
DRInterpreterContext >> receiver [
	^ receiver
]

{ #category : #accessing }
DRInterpreterContext >> receiver: anObject [
	receiver := anObject
]

{ #category : #accessing }
DRInterpreterContext >> stack [
	
	| stack |
	stack := {self}.
	self parent ifNotNil: [ :theParent | stack := stack, theParent stack].
	^ stack
]

{ #category : #temporaries }
DRInterpreterContext >> tempNamed: aName put: aValue [ 
	
	temporaries at: aName put: aValue
]

{ #category : #accessing }
DRInterpreterContext >> temporaryNamed: aName [ 
	
	^ temporaries at: aName ifAbsent: [ block home temporaryNamed: aName ]
]
