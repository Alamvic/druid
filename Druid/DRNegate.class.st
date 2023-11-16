Class {
	#name : #DRNegate,
	#superclass : #DRUnaryInstruction,
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

{ #category : #SCCP }
DRNegate >> sccpOperateOnLattice: operand [
	
	^ operand negated
]
