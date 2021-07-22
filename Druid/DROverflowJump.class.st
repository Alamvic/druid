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
DROverflowJump >> prettyPrint [
	
	^ 'Jump if overflow -> ', self trueBranch id asString, ' if false -> ', self falseBranch id asString
]
