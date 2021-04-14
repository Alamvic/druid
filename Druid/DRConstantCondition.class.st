Class {
	#name : #DRConstantCondition,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRConstantCondition >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitConstantCondition: self
]

{ #category : #printing }
DRConstantCondition >> opcode [
	
	^ operands first name
]
