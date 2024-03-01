Class {
	#name : #DRRotate,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRRotate >> acceptVisitor: aVisitor [

	^ aVisitor visitLeftRotate: self
]
