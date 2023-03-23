Class {
	#name : #DRClosureActivation,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRClosureActivation >> acceptVisitor: aVisitor [

	^ aVisitor visitClosureActivation: self
]

{ #category : #testing }
DRClosureActivation >> isMandatoryInstruction [

	^ true
]

{ #category : #printing }
DRClosureActivation >> opcode [

	^ 'CODE ACTIVATION'
]
