Class {
	#name : #DRDivision,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRDivision >> acceptVisitor: aVisitor [

	^ aVisitor visitDivision: self
]

{ #category : #printing }
DRDivision >> opcode [
	
	^ 'DIV'
]
