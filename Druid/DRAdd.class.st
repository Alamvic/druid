Class {
	#name : #DRAdd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRAdd >> acceptVisitor: aVisitor [

	^ aVisitor visitAddition: self
]

{ #category : #printing }
DRAdd >> opcode [
	
	^ 'ADD'
]
