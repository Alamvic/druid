Class {
	#name : #DRAdd,
	#superclass : #Object,
	#instVars : [
		'leftOperand',
		'rightOperand'
	],
	#category : #Druid
}

{ #category : #accessing }
DRAdd >> leftOperand [
	^ leftOperand
]

{ #category : #accessing }
DRAdd >> leftOperand: anObject [
	leftOperand := anObject
]

{ #category : #accessing }
DRAdd >> rightOperand [
	^ rightOperand
]

{ #category : #accessing }
DRAdd >> rightOperand: anObject [
	rightOperand := anObject
]
