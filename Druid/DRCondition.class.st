Class {
	#name : #DRCondition,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
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

{ #category : #converting }
DRCondition >> reversed [
	
	self subclassResponsibility
]

{ #category : #accessing }
DRCondition >> type [
	
	^ DRBooleanType new
]
