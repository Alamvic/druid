Class {
	#name : #DROverflowJump,
	#superclass : #DRConditionalJump,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DROverflowJump >> acceptVisitor: aVisitor [

	^ aVisitor visitOverflowJump: self
]

{ #category : #visiting }
DROverflowJump >> prettyConditionPrint [

	^ 'overflow'
]
