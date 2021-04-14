Class {
	#name : #DRStore,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRStore >> acceptVisitor: aVisitor [
	
	^ aVisitor visitStore: self
]