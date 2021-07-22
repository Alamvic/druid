Class {
	#name : #DRCondition,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
	#category : #'Druid-IR'
}

{ #category : #reversing }
DRCondition >> reversed [
	
	^ DRLessOrEqualsThanComparison operands: operands reversed result: result
]
