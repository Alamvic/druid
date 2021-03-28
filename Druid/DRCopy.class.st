Class {
	#name : #DRCopy,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRCopy >> acceptVisitor: aVisitor [
	
	^ aVisitor visitCopy: self
]
