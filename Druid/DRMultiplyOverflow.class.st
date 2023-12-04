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

{ #category : #transforming }
DRMultiplyOverflow >> negated [

	^ DRNoMultiplyOverflow new
]

{ #category : #printing }
DRMultiplyOverflow >> opcode [
	
	^ 'Multiply Overflow'
]
