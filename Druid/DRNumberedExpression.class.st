Class {
	#name : #DRNumberedExpression,
	#superclass : #Object,
	#instVars : [
		'operation',
		'operands'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #comparing }
DRNumberedExpression >> = anObject [

	self class = anObject class ifTrue: [ 
		self operation = anObject operation ifTrue: [
			self operands = anObject operands ifTrue: [
				^ true. ]]].
	^ false.
]

{ #category : #comparing }
DRNumberedExpression >> hash [

	^ self operation hash bitXor: self operands hash
]

{ #category : #comparing }
DRNumberedExpression >> operands [
	^ operands
]

{ #category : #comparing }
DRNumberedExpression >> operands: op [
	operands := op
]

{ #category : #comparing }
DRNumberedExpression >> operation [
	^ operation
]

{ #category : #comparing }
DRNumberedExpression >> operation: op [
	operation := op
]
