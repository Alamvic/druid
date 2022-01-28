Class {
	#name : #DRAdd,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRAdd >> acceptVisitor: aVisitor [

	^ aVisitor visitAddition: self
]

{ #category : #testing }
DRAdd >> isAdd [
	
	^ true
]

{ #category : #printing }
DRAdd >> opcode [
	
	^ 'ADD'
]

{ #category : #printing }
DRAdd >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand + operand2
]

{ #category : #'constant folding' }
DRAdd >> simpleConstantFold [
	
	^ self
		sccpOperateOnLattice: self operand1 simpleConstantFold
		and: self operand2 simpleConstantFold
]
