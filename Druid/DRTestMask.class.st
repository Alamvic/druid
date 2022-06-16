Class {
	#name : #DRTestMask,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRTestMask >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitTestMask: self inBranch: parentBranch
]

{ #category : #visiting }
DRTestMask >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpZero:
]

{ #category : #visiting }
DRTestMask >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpNonZero:
]

{ #category : #printing }
DRTestMask >> opcode [
	
	^ 'Test'
]

{ #category : #converting }
DRTestMask >> reversed [

	"Returns the same condition but with reversed operands: Test x y --> Test y x"
	^ DRTestMask new
]

{ #category : #SCCP }
DRTestMask >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand anyMask: operand2
]
