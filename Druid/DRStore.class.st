Class {
	#name : #DRStore,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRStore >> acceptVisitor: aVisitor [
	
	^ aVisitor visitStore: self
]

{ #category : #visiting }
DRStore >> isMandatoryInstruction [ 

	^ true
]

{ #category : #visiting }
DRStore >> isStore [ 

	^ true
]

{ #category : #visiting }
DRStore >> opcode [
	
	^ 'Store'
]
