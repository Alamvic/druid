Class {
	#name : #DRJitCompileTimeExpression,
	#superclass : #DRObjectReferenceValue,
	#instVars : [
		'expression'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRJitCompileTimeExpression class >> expression: anExpression [

	^ self new
		expression: anExpression;
		yourself
]

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

{ #category : #testing }
DRJitCompileTimeExpression >> isJITCompileTimeExpression: aString [ 
	
	^ aString = expression
]

{ #category : #testing }
DRJitCompileTimeExpression >> isJitCompileTimeExpression [
	
	^ true
]

{ #category : #testing }
DRJitCompileTimeExpression >> jitCompileTimeExpression [

	^ expression
]

{ #category : #accessing }
DRJitCompileTimeExpression >> name [

	^ expression isString ifTrue: [ expression ] ifFalse: [ expression formattedCode ]
]

{ #category : #types }
DRJitCompileTimeExpression >> type [
	
	"Assume integer for now"
	^ DRIntegerType new
]
