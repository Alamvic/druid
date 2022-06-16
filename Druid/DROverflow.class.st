Class {
	#name : #DROverflow,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DROverflow >> acceptVisitor: aVisitor withBranch: aDRBranchIfCondition [ 
	 
	^ aVisitor visitOverflow: self inBranch: aDRBranchIfCondition
]

{ #category : #'rtl-translation' }
DROverflow >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpNoOverflow:
]

{ #category : #'rtl-translation' }
DROverflow >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpOverflow:
]

{ #category : #testing }
DROverflow >> isOverflow [
	
	^ true
]

{ #category : #printing }
DROverflow >> opcode [
	
	^ 'Overflow'
]

{ #category : #printing }
DROverflow >> prettyPrintWithBranch: aDRBranchIfCondition [

	^ self opcode
]

{ #category : #printing }
DROverflow >> reversed [

	"Returns the same condition but with reversed operands: x = y --> y = x"
1halt.
	^ DREqualsThanComparison new
]

{ #category : #'as yet unclassified' }
DROverflow >> sccpEvaluateFor: sccp branch: aBranch [

	"We cannot know statically if an overflow will happen.
	Or can we? ;)"
	^ sccp bottom
]
