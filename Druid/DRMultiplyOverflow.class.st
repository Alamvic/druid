Class {
	#name : #DRMultiplyOverflow,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRMultiplyOverflow >> acceptVisitor: aVisitor [
	
	1halt.
	^ aVisitor visitEqualsComparison: self
]

{ #category : #visiting }
DRMultiplyOverflow >> acceptVisitor: aVisitor withBranch: aDRBranchIfCondition [ 
	
	^ aVisitor visitMultiplyOverflow: self inBranch: aDRBranchIfCondition
]

{ #category : #'rtl-translation' }
DRMultiplyOverflow >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpMultiplyNoOverflow:
]

{ #category : #'rtl-translation' }
DRMultiplyOverflow >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpMultiplyOverflow:
]

{ #category : #testing }
DRMultiplyOverflow >> isOverflow [
	
	^ true
]

{ #category : #printing }
DRMultiplyOverflow >> opcode [
	
	^ 'Multiply Overflow'
]

{ #category : #printing }
DRMultiplyOverflow >> prettyPrintWithBranch: aDRBranchIfCondition [

	^ self opcode
]

{ #category : #printing }
DRMultiplyOverflow >> reversed [

	"Returns the same condition but with reversed operands: x = y --> y = x"
1halt.
	^ DREqualsThanComparison operands: operands reversed result: result
]

{ #category : #'as yet unclassified' }
DRMultiplyOverflow >> sccpEvaluateFor: sccp branch: aBranch [

	"We cannot know statically if an overflow will happen.
	Or can we? ;)"
	^ sccp bottom
]

{ #category : #SCCP }
DRMultiplyOverflow >> sccpOperateOnLattice: operand and: operand2 [
	1halt.
	^ operand = operand2
]
