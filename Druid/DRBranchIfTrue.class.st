Class {
	#name : #DRBranchIfTrue,
	#superclass : #DRAbstractBranch,
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRBranchIfTrue >> condition [
	
	^ self operand1
]

{ #category : #testing }
DRBranchIfTrue >> isBranchIfTrue [

	^ true
]

{ #category : #printing }
DRBranchIfTrue >> prettyConditionPrint [

	^ self condition result name , '= true'
]

{ #category : #SCCP }
DRBranchIfTrue >> sccpEvaluateConditionFor: sccp [

	^ self condition sccpLatticeValueFor: sccp
]
