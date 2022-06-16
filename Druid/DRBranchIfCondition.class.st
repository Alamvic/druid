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

	^ aVisitor visitConditionalJump: self
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

{ #category : #SCCP }
DRBranchIfCondition >> sccpEvaluateConditionFor: sccp [

	^ self condition
		sccpEvaluateFor: sccp
		branch: self
]

{ #category : #SCCP }
DRBranchIfCondition >> sccpEvaluateFor: sccp [

	| conditionLattice jumpTarget |
	conditionLattice := self sccpEvaluateConditionFor: sccp.

	"If we know nothing, do nothing yet"
	conditionLattice = sccp top ifTrue: [ ^ self ].

	conditionLattice = sccp bottom ifTrue: [ 
		self targets do: [ :t | 
			sccp addToEdgeCFGWorklistConditionalJump: self targetting: t ].
		^ self ].

	conditionLattice isNullValue ifTrue: [ ^ self ].

	jumpTarget := conditionLattice value
		              ifTrue: [ self trueBranch ]
		              ifFalse: [ self falseBranch ].
	sccp addToEdgeCFGWorklistConditionalJump: self targetting: jumpTarget
]
