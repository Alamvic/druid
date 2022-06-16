Class {
	#name : #DRMultiplyOverflow,
	#superclass : #DROverflow,
	#category : #'Druid-IR'
}

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

{ #category : #printing }
DRMultiplyOverflow >> opcode [
	
	^ 'Multiply Overflow'
]

{ #category : #printing }
DRMultiplyOverflow >> reversed [

	"Returns the same condition but with reversed operands: x = y --> y = x"
1halt.
	^ DREqualsThanComparison new
]
