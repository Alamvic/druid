Class {
	#name : #DRBranchIfTrue,
	#superclass : #DRAbstractBranch,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBranchIfTrue >> acceptVisitor: aVisitor [

	^ aVisitor visitBranchIfTrue: self
]

{ #category : #testing }
DRBranchIfTrue >> isBranchIfTrue [

	^ true
]

{ #category : #printing }
DRBranchIfTrue >> prettyConditionPrint [

	^ self operand1 result name , '= true'
]

{ #category : #SCCP }
DRBranchIfTrue >> sccpEvaluateConditionFor: sccp [

	^ self operand1 sccpLatticeValueFor: sccp
]
