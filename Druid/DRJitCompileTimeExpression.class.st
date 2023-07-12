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

{ #category : #sccp }
DRJitCompileTimeExpression class >> tryParseOperandForSCCP: aDRValue [

	| result |
	result := aDRValue asJitCompileTimeExpression.
	result ifNil: [ ^ DRSCCP bottom ].
	^ result
]

{ #category : #operations }
DRJitCompileTimeExpression >> * aDRJitCompileTimeExpression [

	^ self mergeExpression: '*' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> + aDRJitCompileTimeExpression [

	^ self mergeExpression: '+' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> , aDRJitCompileTimeExpression [

	^ self mergeExpression: '' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> - aDRJitCompileTimeExpression [ 
	
	^ self mergeExpression: '-' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> < aDRJitCompileTimeExpression [

	^ self mergeExpression: '<' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> << aDRJitCompileTimeExpression [

	^ self mergeExpression: '<<' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> > aDRJitCompileTimeExpression [

	^ self mergeExpression: '>' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> >> aDRJitCompileTimeExpression [

	^ self mergeExpression: '>>' with: aDRJitCompileTimeExpression
]

{ #category : #visiting }
DRJitCompileTimeExpression >> acceptVisitor: aVisitor [

	^ aVisitor visitJITCompileTimeExpression: self
]

{ #category : #converting }
DRJitCompileTimeExpression >> asJitCompileTimeExpression [

	^ self
]

{ #category : #'bit manipulation' }
DRJitCompileTimeExpression >> bitAnd: aDRJitCompileTimeExpression [

	^ self mergeExpression: 'bitAnd:' with: aDRJitCompileTimeExpression
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
DRJitCompileTimeExpression >> isJITCompileTimeExpression [
	
	^ true
]

{ #category : #testing }
DRJitCompileTimeExpression >> isJITCompileTimeExpression: aString [ 
	
	^ aString = expression
]

{ #category : #testing }
DRJitCompileTimeExpression >> jitCompileTimeExpression [

	^ expression
]

{ #category : #copying }
DRJitCompileTimeExpression >> mergeExpression: operand with: aDRJitCompileTimeExpression [

	^ self class expression: (self
			   mergeExpressions: self expression
			   op: operand
			   other: aDRJitCompileTimeExpression expression)
]

{ #category : #operations }
DRJitCompileTimeExpression >> mergeExpressions: expression1 op: op other: expression2 [

	^ '(' , expression1 , ' ' , op , ' ' , expression2 , ')'
]

{ #category : #accessing }
DRJitCompileTimeExpression >> name [

	^ expression isString ifTrue: [ expression ] ifFalse: [ expression formattedCode ]
]

{ #category : #printing }
DRJitCompileTimeExpression >> printResult [

	^ expression
]

{ #category : #operations }
DRJitCompileTimeExpression >> sccpEquals: aDRJitCompileTimeExpression [

	^ self mergeExpression: '=' with: aDRJitCompileTimeExpression
]

{ #category : #types }
DRJitCompileTimeExpression >> type [
	
	"Assume integer for now"
	^ DRIntegerType new
]
