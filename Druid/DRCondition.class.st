Class {
	#name : #DRCondition,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
	#category : #'Druid-IR'
}

{ #category : #converting }
DRCondition >> reversed [
	
	self subclassResponsibility
]
