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
	
	^ DRLessOrEqualsThanComparison operands: operands reversed result: result
]

{ #category : #SCCP }
DRGreaterThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand > operand2
]
