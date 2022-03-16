Class {
	#name : #DRLessOrEqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLessOrEqualsThanComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitLessOrEqualsThanComparison: self
]

{ #category : #printing }
DRLessOrEqualsThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking lessOrEquals, the we should return greaterThan"
	
	^ #JumpGreater:
]

{ #category : #printing }
DRLessOrEqualsThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking lessOrEquals, the we should return greaterThan"
	
	^ #JumpLessOrEqual:
]

{ #category : #printing }
DRLessOrEqualsThanComparison >> opcode [
	
	^ 'LessOrEqualsThan'
]

{ #category : #printing }
DRLessOrEqualsThanComparison >> reversed [
	
	^ DRGreaterOrEqualsThanComparison operands: operands reversed result: result
]

{ #category : #SCCP }
DRLessOrEqualsThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand <= operand2
]
