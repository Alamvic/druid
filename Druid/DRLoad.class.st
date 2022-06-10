Class {
	#name : #DRLoad,
	#superclass : #DRInstruction,
	#instVars : [
		'size'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoad >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLoad: self
]

{ #category : #visiting }
DRLoad >> isLoad [

	^ true
]

{ #category : #printing }
DRLoad >> opcode [
	
	^ 'LOAD'
]

{ #category : #accessing }
DRLoad >> operands: operandList [

	size := operandList first value.
	super operands: operandList allButFirst
]

{ #category : #accessing }
DRLoad >> size [

	^ size
]

{ #category : #accessing }
DRLoad >> type [
	
	^ DRIntegerType new
]
