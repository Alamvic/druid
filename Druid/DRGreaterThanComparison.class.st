Class {
	#name : #DRGreaterThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRGreaterThanComparison >> acceptVisitor: aVisitor withBranch: aDRBranchIfCondition [ 
	
	^ aVisitor visitGreaterThan: self inBranch: aDRBranchIfCondition
]

{ #category : #visiting }
DRGreaterThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking greaterOrEquals, the we should return lessThan"
	
	^ #JumpLessOrEqual:
]

{ #category : #visiting }
DRGreaterThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking greaterOrEquals, the we should return lessThan"
	
	^ #JumpGreater:
]

{ #category : #printing }
DRGreaterThanComparison >> opcode [
	
	^ 'GreaterThan'
]

{ #category : #reversing }
DRGreaterThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x > y --> y < x"
	^ DRLessThanComparison new
]

{ #category : #SCCP }
DRGreaterThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand > operand2
]
