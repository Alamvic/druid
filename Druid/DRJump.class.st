Class {
	#name : #DRJump,
	#superclass : #DRInstruction,
	#instVars : [
		'target'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRJump >> acceptVisitor: aVisitor [
	
	^ aVisitor visitJump: self
]

{ #category : #accessing }
DRJump >> target [
	^ target
]

{ #category : #accessing }
DRJump >> target: aBasicBlock [ 
	
	target := aBasicBlock
]

{ #category : #accessing }
DRJump >> targets [
	
	^ { target }
]
