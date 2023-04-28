Class {
	#name : #DRSubtract,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRSubtract >> acceptVisitor: aVisitor [
	
	^ aVisitor visitSubstract: self
]

{ #category : #testing }
DRSubtract >> isSubtract [

	^ true
]

{ #category : #accessing }
DRSubtract >> minuend [
	
	^ self operand1
]

{ #category : #printing }
DRSubtract >> opcode [
	
	^ 'SUB'
]

{ #category : #printing }
DRSubtract >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand - operand2
]

{ #category : #accessing }
DRSubtract >> subtrahend [
	
	^ self operand2
]
