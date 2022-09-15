Class {
	#name : #DROverflow,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DROverflow >> acceptVisitor: aVisitor withBranch: aDRBranchIfCondition [ 
	 
	^ aVisitor visitOverflow: self inBranch: aDRBranchIfCondition
]

{ #category : #testing }
DROverflow >> isMultiplyOverflow [

	^ false
]

{ #category : #testing }
DROverflow >> isOverflow [
	
	^ true
]

{ #category : #transforming }
DROverflow >> negated [

	^ DRNoOverflow new
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
