Class {
	#name : #DRMultiplyOverflowJump,
	#superclass : #DRAbstractBranch,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRMultiplyOverflowJump >> acceptVisitor: aVisitor [

	^ aVisitor visitMultiplyOverflowJump: self
]

{ #category : #visiting }
DRMultiplyOverflowJump >> prettyConditionPrint [

	^ 'multiply overflow'
]
