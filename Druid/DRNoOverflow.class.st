Class {
	#name : #DRNoOverflow,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNoOverflow >> acceptVisitor: aVisitor withBranch: aDRBranchIfCondition [ 
	 
	^ aVisitor visitNoOverflow: self inBranch: aDRBranchIfCondition
]

{ #category : #testing }
DRNoOverflow >> isNoMultiplyOverflow [
	
	^ false
]

{ #category : #testing }
DRNoOverflow >> isNoOverflow [

	^ true
]

{ #category : #transforming }
DRNoOverflow >> negated [
	
	^ DROverflow new
]

{ #category : #printing }
DRNoOverflow >> opcode [
	
	^ 'No Overflow'
]

{ #category : #SCCP }
DRNoOverflow >> sccpEvaluateFor: sccp branch: aBranch [

	"We cannot know statically if an overflow will happen.
	Or can we? ;)"
	^ sccp bottom
]
