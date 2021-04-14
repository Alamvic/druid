Class {
	#name : #DRCondition,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
	#category : #'Druid-IR'
}

{ #category : #'rtl-translation' }
DRCondition >> cogitFalseConditionCode [
	
	self subclassResponsibility
]

{ #category : #accessing }
DRCondition >> simplify [
	
	^ self
]
