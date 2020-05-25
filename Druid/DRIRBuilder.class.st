Class {
	#name : #DRIRBuilder,
	#superclass : #Object,
	#instVars : [
		'instructions'
	],
	#category : #Druid
}

{ #category : #building }
DRIRBuilder >> add: aValue and: anotherValue [
	
	^ DRAdd new
		leftOperand: aValue;
		rightOperand: anotherValue;
		yourself
]

{ #category : #accessing }
DRIRBuilder >> appendInstruction: anInstruction [ 
	
	instructions add: anInstruction
]

{ #category : #initialization }
DRIRBuilder >> initialize [

	super initialize.
	instructions := OrderedCollection new
]

{ #category : #building }
DRIRBuilder >> newConstant: aValue [

	^ DRConstant value: aValue
]

{ #category : #building }
DRIRBuilder >> push: aValue [
	
	| pushInstruction |
	pushInstruction := DRPush value: aValue.
	pushInstruction appendOn: self.

	^ DRInvalid new
]
