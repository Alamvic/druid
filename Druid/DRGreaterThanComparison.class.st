Class {
	#name : #DRGreaterThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRGreaterThanComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitGreaterThanComparison: self
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

	^ DRLessThanComparison operands: operands reversed result: result
]

{ #category : #SCCP }
DRGreaterThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand > operand2
]
