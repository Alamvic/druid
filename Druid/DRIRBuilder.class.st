Class {
	#name : #DRIRBuilder,
	#superclass : #Object,
	#instVars : [
		'instructions'
	],
	#category : #'Druid-Instructions'
}

{ #category : #building }
DRIRBuilder >> add: aValue and: anotherValue [
	
	^ self appendInstruction: (DRAdd new
		leftOperand: aValue;
		rightOperand: anotherValue;
		yourself)
]

{ #category : #accessing }
DRIRBuilder >> appendInstruction: anInstruction [ 
	
	instructions add: anInstruction.
	^ anInstruction
]

{ #category : #initialization }
DRIRBuilder >> initialize [

	super initialize.
	instructions := OrderedCollection new
]

{ #category : #building }
DRIRBuilder >> loadAddress: aValue [
	
	^ self appendInstruction: (DRLoad value: (DRIndirection value: aValue))
]

{ #category : #building }
DRIRBuilder >> newConstant: aValue [

	^ DRConstant value: aValue
]

{ #category : #building }
DRIRBuilder >> push: aValue [
	
	self appendInstruction: (DRPush value: aValue).
	^ DRInvalid new
]
