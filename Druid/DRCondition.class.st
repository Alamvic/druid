Class {
	#name : #DRCondition,
	#superclass : #Object,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRCondition class >> isEqualsThanComparison [

	^ false
]

{ #category : #visiting }
DRCondition >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	self subclassResponsibility
]

{ #category : #testing }
DRCondition >> isCommutative [
	
	^ false
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
DRCondition >> isEqualsThanComparison [
	
	^ false
]

{ #category : #testing }
DRCondition >> isGreaterOrEqualThanComparison [

	^ false
]

{ #category : #testing }
DRCondition >> isGreaterThanComparison [
	
	^ false
]

{ #category : #testing }
DRCondition >> isLessOrEqualThanComparison [
	
	^ false
]

{ #category : #testing }
DRCondition >> isLessThanComparison [
	
	^ false
]

{ #category : #testing }
DRCondition >> isNoOverflow [
	
	^ false
]

{ #category : #testing }
DRCondition >> isNoTestMask [
	
	^ false
]

{ #category : #testing }
DRCondition >> isNotEqualsThanComparison [
	
	^ false
]

{ #category : #testing }
DRCondition >> isOverflow [

	^ false
]

{ #category : #testing }
DRCondition >> isTestMask [
	
	^ false
]

{ #category : #testing }
DRCondition >> isTypeOf [
	^ false
]

{ #category : #transforming }
DRCondition >> negated [
	
	^ self subclassResponsibility
]

{ #category : #accessing }
DRCondition >> operandsForInstruction: aDRBranchIfCondition [ 
	
	^ aDRBranchIfCondition operands
]

{ #category : #printing }
DRCondition >> prettyPrintWithBranch: aDRBranchIfCondition [

	| conditionResult conditionResultName conditionComparisonValueName |
	conditionResult := aDRBranchIfCondition operand1 result.
	conditionResultName := conditionResult
		                       ifNotNil: [ conditionResult name , ' ' ]
		                       ifNil: [ '' ].
	conditionComparisonValueName := aDRBranchIfCondition operand2
		                                ifNotNil: [ ' ' , aDRBranchIfCondition operand2 name ]
		                                ifNil: [ '' ].
	^ conditionResultName , self opcode , conditionComparisonValueName
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

{ #category : #SCCP }
DRCondition >> sccpOperateOnLattice: operand and: operand2 [

	self subclassResponsibility
]

{ #category : #accessing }
DRCondition >> type [
	
	^ DRBooleanType new
]
