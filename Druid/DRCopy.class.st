Class {
	#name : #DRCopy,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRCopy >> acceptVisitor: aVisitor [
	
	^ aVisitor visitCopy: self
]

{ #category : #printing }
DRCopy >> isCopy [ 

	^true 
]

{ #category : #printing }
DRCopy >> operands: aCollection [ 
	operands := aCollection
]

{ #category : #printing }
DRCopy >> prettyPrint [
	
	^ result name, ' := ', operands first name asString
]

{ #category : #SCCP }
DRCopy >> sccpEvaluateFor: sccp [

	^ sccp value: operands first
]
