Class {
	#name : #DRAdd,
	#superclass : #DRInstruction,
	#instVars : [
		'leftOperand',
		'rightOperand'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRAdd >> leftOperand [
	^ leftOperand
]

{ #category : #accessing }
DRAdd >> leftOperand: anObject [
	leftOperand := anObject
]

{ #category : #printing }
DRAdd >> mnemonic [
	
	^ 'ADD'
]

{ #category : #'as yet unclassified' }
DRAdd >> operands [

	^ { leftOperand . rightOperand }
]

{ #category : #accessing }
DRAdd >> result [
	
	^ 't2'
]

{ #category : #accessing }
DRAdd >> rightOperand [
	^ rightOperand
]

{ #category : #accessing }
DRAdd >> rightOperand: anObject [
	rightOperand := anObject
]
