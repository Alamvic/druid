Class {
	#name : 'DROverflow',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DROverflow >> acceptVisitor: aVisitor withBranch: aDRBranchIfCondition [ 
	 
	^ aVisitor visitOverflow: self inBranch: aDRBranchIfCondition
]

{ #category : 'testing' }
DROverflow >> isMultiplyOverflow [

	^ false
]

{ #category : 'testing' }
DROverflow >> isOverflow [
	
	^ true
]

{ #category : 'transforming' }
DROverflow >> negated [

	^ DRNoOverflow new
]

{ #category : 'printing' }
DROverflow >> opcode [
	
	^ 'Overflow'
]

{ #category : 'accessing' }
DROverflow >> operandsForInstruction: aDRBranchIfCondition [
	"Overflows have a single operand: the instruction that may cause overflow"

	^ { aDRBranchIfCondition operand1 }
]

{ #category : 'printing' }
DROverflow >> prettyPrintWithBranch: aDRBranchIfCondition [

	^ self opcode
]

{ #category : 'as yet unclassified' }
DROverflow >> sccpEvaluateFor: sccp branch: aBranch [

	"We cannot know statically if an overflow will happen.
	Or can we? ;)"
	^ sccp bottom
]
