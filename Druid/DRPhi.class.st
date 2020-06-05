Class {
	#name : #DRPhi,
	#superclass : #DRValue,
	#instVars : [
		'values'
	],
	#category : #'Druid-Instructions'
}

{ #category : #printing }
DRPhi >> mnemonic [
	
	^ 'PHI'
]

{ #category : #'as yet unclassified' }
DRPhi >> operands [
	
	^ values
]

{ #category : #accessing }
DRPhi >> values [
	^ values
]

{ #category : #accessing }
DRPhi >> values: anObject [
	values := anObject
]
