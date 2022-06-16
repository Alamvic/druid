Class {
	#name : #DRCondition,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

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

	^ aDRBranchIfCondition operand1 result asString , ' ' , self opcode
	  , ' ' , aDRBranchIfCondition operand1 asString
]

{ #category : #converting }
DRCondition >> reversed [
	
	self subclassResponsibility
]

{ #category : #'as yet unclassified' }
DRCondition >> sccpEvaluateFor: sccp branch: aBranch [

	| operand1 operand2 |
	operand1 := sccp latticeValue: aBranch operand1.
	operand2 := sccp latticeValue: aBranch operand2.

	^ self
		  sccpLatticeValueFor: sccp
		  latticeOperand1: operand1
		  latticeOperand2: operand2
]

{ #category : #accessing }
DRCondition >> type [
	
	^ DRBooleanType new
]
