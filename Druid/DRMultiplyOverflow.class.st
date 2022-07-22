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
DRMultiplyOverflow >> isMultiplyOverflow [

	^ true
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
