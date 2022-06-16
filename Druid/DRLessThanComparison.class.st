Class {
	#name : #DRLessThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLessThanComparison >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitLessThan: self inBranch: parentBranch
]

{ #category : #visiting }
DRLessThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking lessOrEquals, the we should return greaterThan"
	^ #JumpGreaterOrEqual:
]

{ #category : #visiting }
DRLessThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking lessOrEquals, the we should return greaterThan"
	^ #JumpLess:
]

{ #category : #printing }
DRLessThanComparison >> opcode [
	
	^ 'LessThan'
]

{ #category : #printing }
DRLessThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x < y --> y > x"
	^ DRGreaterThanComparison new
]

{ #category : #SCCP }
DRLessThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand < operand2
]
