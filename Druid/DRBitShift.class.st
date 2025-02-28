Class {
	#name : 'DRBitShift',
	#superclass : 'DRArithmeticInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBitShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBitShift: self
]

{ #category : 'testing' }
DRBitShift >> isLogicalRightShift [
	
	^ true
]

{ #category : 'printing' }
DRBitShift >> opcode [
	
	^ 'BitShift'
]

{ #category : 'printing' }
DRBitShift >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand >> operand2
]
