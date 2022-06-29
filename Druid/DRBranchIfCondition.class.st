Class {
	#name : #DRBranchIfCondition,
	#superclass : #DRAbstractBranch,
	#instVars : [
		'condition'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBranchIfCondition >> acceptVisitor: aVisitor [

	^ aVisitor visitBranchIfCondition: self
]

{ #category : #accessing }
DRBranchIfCondition >> condition [
	
	^ condition
]

{ #category : #accessing }
DRBranchIfCondition >> condition: aCondition [

	condition := aCondition
]

{ #category : #accessing }
DRBranchIfCondition >> isBranchIfCondition [ 

	^ true
]

{ #category : #accessing }
DRBranchIfCondition >> operands: operandList [

	condition := operandList first value.
	super operands: operandList allButFirst
]

{ #category : #visiting }
DRBranchIfCondition >> prettyConditionPrint [

	^ self condition prettyPrintWithBranch: self
]

{ #category : #transforming }
DRBranchIfCondition >> reversed [

	^ DRBranchIfCondition new
		operands: { self condition reversed }, self operands reversed;
		trueBranch: self trueBranch;
		falseBranch: self falseBranch;
		yourself
]

{ #category : #SCCP }
DRBranchIfCondition >> sccpEvaluateConditionFor: sccp [

	^ self condition
		sccpEvaluateFor: sccp
		branch: self
]
