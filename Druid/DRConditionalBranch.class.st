Class {
	#name : #DRConditionalBranch,
	#superclass : #DRInstruction,
	#instVars : [
		'trueTarget',
		'falseTarget'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRConditionalBranch >> falseTarget [
	^ falseTarget
]

{ #category : #accessing }
DRConditionalBranch >> falseTarget: anObject [
	falseTarget := anObject
]

{ #category : #printing }
DRConditionalBranch >> mnemonic [

	^ 'BRANCH'
]

{ #category : #'as yet unclassified' }
DRConditionalBranch >> operands [
	self shouldBeImplemented.
]

{ #category : #'as yet unclassified' }
DRConditionalBranch >> printOn: aStream [

	aStream
		nextPutAll: self mnemonic;
		nextPutAll: ' ifTrue: ';
		nextPutAll: trueTarget id asString;
		nextPutAll: ' ifFalse: ';
		nextPutAll: falseTarget id asString;
		yourself
]

{ #category : #accessing }
DRConditionalBranch >> trueTarget [
	^ trueTarget
]

{ #category : #accessing }
DRConditionalBranch >> trueTarget: anObject [
	trueTarget := anObject
]
