Class {
	#name : #DRBitAnd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBitAnd >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitAnd: self
]

{ #category : #printing }
DRBitAnd >> opcode [
	
	^ 'AND'
]

{ #category : #printing }
DRBitAnd >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand bitAnd: operand2
]
