Class {
	#name : #DRInterpreterContext,
	#superclass : #Object,
	#instVars : [
		'parent',
		'method',
		'receiver',
		'arguments',
		'block'
	],
	#category : #'Druid-Interpreter'
}

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

{ #category : #accessing }
DRInterpreterContext >> receiver [
	^ receiver
]

{ #category : #accessing }
DRInterpreterContext >> receiver: anObject [
	receiver := anObject
]
