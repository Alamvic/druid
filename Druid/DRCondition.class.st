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

{ #category : #converting }
DRCondition >> reversed [
	
	self subclassResponsibility
]
