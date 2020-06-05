Class {
	#name : #DRSend,
	#superclass : #DRInstruction,
	#instVars : [
		'selector',
		'receiver',
		'arguments'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRSend >> arguments [
	^ arguments
]

{ #category : #accessing }
DRSend >> arguments: anObject [
	arguments := anObject
]

{ #category : #accessing }
DRSend >> receiver [
	^ receiver
]

{ #category : #accessing }
DRSend >> receiver: anObject [
	receiver := anObject
]

{ #category : #accessing }
DRSend >> registers [
	
	^ receiver registers, (arguments flatCollect: [ :e | e registers]), result registers
]

{ #category : #accessing }
DRSend >> selector [
	^ selector
]

{ #category : #accessing }
DRSend >> selector: anObject [
	selector := anObject
]
