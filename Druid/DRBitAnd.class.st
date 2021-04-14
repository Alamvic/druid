Class {
	#name : #DRBitAnd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBitAnd >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitAnd: self
]

{ #category : #printing }
DRBitAnd >> opcode [
	
	^ 'AND'
]
