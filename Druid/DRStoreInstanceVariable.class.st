Class {
	#name : #DRStoreInstanceVariable,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRStoreInstanceVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitStoreInstVar: self
]

{ #category : #printing }
DRStoreInstanceVariable >> opcode [
	
	^ 'StoreInstVar'
]
