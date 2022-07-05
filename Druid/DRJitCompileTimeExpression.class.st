Class {
	#name : #DRJitCompileTimeExpression,
	#superclass : #DRObjectReferenceValue,
	#instVars : [
		'expression'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRJitCompileTimeExpression >> acceptVisitor: aVisitor [

	^ aVisitor visitJITCompileTimeExpression: self
]

{ #category : #accessing }
DRJitCompileTimeExpression >> expression [

	^ expression
]

{ #category : #accessing }
DRJitCompileTimeExpression >> expression: anObject [

	expression := anObject
]

{ #category : #testing }
DRJitCompileTimeExpression >> isConstant [

	^ true
]

{ #category : #accessing }
DRJitCompileTimeExpression >> name [

	^ expression
]
