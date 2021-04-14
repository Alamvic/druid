Class {
	#name : #DRNegate,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNegate >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitNegate: self
]

{ #category : #printing }
DRNegate >> opcode [
	
	^ 'Negate'
]
