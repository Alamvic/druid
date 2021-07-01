Class {
	#name : #DRReturn,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRReturn >> acceptVisitor: aVisitor [
	
	^ aVisitor visitReturn: self
]

{ #category : #printing }
DRReturn >> opcode [
	
	^ 'Return'
]
