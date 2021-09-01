Class {
	#name : #DRSubtract,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRSubtract >> acceptVisitor: aVisitor [
	
	^ aVisitor visitSubstract: self
]

{ #category : #printing }
DRSubtract >> opcode [
	
	^ 'SUB'
]

{ #category : #printing }
DRSubtract >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand - operand2
]
