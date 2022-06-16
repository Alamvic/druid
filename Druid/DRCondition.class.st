Class {
	#name : #DRCondition,
	#superclass : #Object,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRCondition >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	self subclassResponsibility
]

{ #category : #testing }
DRCondition >> isComparison [

	^ true
]

{ #category : #testing }
DRCondition >> isComplexCondition [

	^ false
]

{ #category : #testing }
DRCondition >> isOverflow [

	^ false
]

{ #category : #printing }
DRCondition >> prettyPrintWithBranch: aDRBranchIfCondition [

	^ aDRBranchIfCondition operand1 result name , ' ' , self opcode , ' '
	  , aDRBranchIfCondition operand2 name
]

{ #category : #converting }
DRCondition >> reversed [
	
	self subclassResponsibility
]

{ #category : #SCCP }
DRCondition >> sccpEvaluateFor: sccp branch: aBranch [

	| operand1 operand2 |
	operand1 := sccp latticeValue: aBranch operand1.
	operand2 := sccp latticeValue: aBranch operand2.

	^ sccp
		  sccpLatticeValueFor: self
		  latticeOperand1: operand1
		  latticeOperand2: operand2
]

{ #category : #accessing }
DRCondition >> type [
	
	^ DRBooleanType new
]
