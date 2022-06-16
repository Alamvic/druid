Class {
	#name : #DRGreaterOrEqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRGreaterOrEqualsThanComparison >> acceptVisitor: aVisitor withBranch: parentBranch [ 
	
	^ aVisitor visitGreaterOrEqualsThan: self inBranch: parentBranch
]

{ #category : #visiting }
DRGreaterOrEqualsThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking greaterOrEquals, the we should return lessThan"
	
	^ #JumpLess:
]

{ #category : #visiting }
DRGreaterOrEqualsThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking greaterOrEquals, the we should return lessThan"
	
	^ #JumpGreaterOrEqual:
]

{ #category : #printing }
DRGreaterOrEqualsThanComparison >> opcode [
	
	^ 'GreaterOrEquals'
]

{ #category : #reversing }
DRGreaterOrEqualsThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x >= y --> y <= x"
	^ DRLessOrEqualsThanComparison new
]

{ #category : #SCCP }
DRGreaterOrEqualsThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand >= operand2
]
