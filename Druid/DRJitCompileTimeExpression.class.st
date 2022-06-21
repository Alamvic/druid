Class {
	#name : #DRJitCompileTimeExpression,
	#superclass : #DRObjectReferenceValue,
	#instVars : [
		'expression'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRJitCompileTimeExpression >> expression [

	^ expression
]

{ #category : #accessing }
DRJitCompileTimeExpression >> expression: anObject [

	expression := anObject
]

{ #category : #accessing }
DRJitCompileTimeExpression >> name [
	
	^ expression
]
