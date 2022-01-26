Class {
	#name : #DRCondition,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
	#category : #'Druid-IR'
}

{ #category : #interpretation }
DRCondition >> lookupClass [
	
	^ Boolean 
]

{ #category : #converting }
DRCondition >> reversed [
	
	self subclassResponsibility
]
