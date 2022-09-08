Class {
	#name : #DRStore,
	#superclass : #DRInstruction,
	#instVars : [
		'size'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRStore >> acceptVisitor: aVisitor [
	
	^ aVisitor visitStore: self
]

{ #category : #testing }
DRStore >> canMoveOutsideLoopWith: backJump [

	^ false
]

{ #category : #visiting }
DRStore >> isMandatoryInstruction [ 

	^ true
]

{ #category : #visiting }
DRStore >> isStore [ 

	^ true
]

{ #category : #visiting }
DRStore >> opcode [
	
	^ 'Store'
]

{ #category : #accessing }
DRStore >> operands: operandList [

	size := operandList first value.
	super operands: operandList allButFirst
]

{ #category : #accessing }
DRStore >> size [

	^ size
]
