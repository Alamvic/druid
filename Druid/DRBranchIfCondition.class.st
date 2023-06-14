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

{ #category : #'jit-compile-time' }
DRBranchIfCondition >> conditionAsJitCompileTimeExpression [

	| fullExpression |
	fullExpression := self operand1 asJitCompileTimeExpression expression
	                  , self condition asJitCompileTimeExpressionString
	                  , self operand2 asJitCompileTimeExpression expression.

	^ fullExpression asDRValue asJitCompileTimeExpression
]

{ #category : #accessing }
DRBranchIfCondition >> hasOverflowCheck [

	^ condition isOverflow
]

{ #category : #accessing }
DRBranchIfCondition >> isBranchIfCondition [ 

	^ true
]

{ #category : #testing }
DRBranchIfCondition >> isJITCompileTimeExpression [

	^ operands allSatisfy: [ :e | e isJITCompileTimeExpression ]
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

	(conditionLattice isNullValue or: [ conditionLattice isJITCompileTimeExpression ]) ifTrue: [ ^ self ].

	jumpTarget := conditionLattice value
		              ifTrue: [ self trueBranch ]
		              ifFalse: [ self falseBranch ].
	sccp addToEdgeCFGWorklistConditionalJump: self targetting: jumpTarget
]
