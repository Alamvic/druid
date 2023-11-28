Class {
	#name : #DRJitCompileTimeExpression,
	#superclass : #DRValue,
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

{ #category : #arithmetic }
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

{ #category : #comparing }
DRJitCompileTimeExpression >> >= aDRJitCompileTimeExpression [

	^ self mergeExpression: '>=' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> >> aDRJitCompileTimeExpression [

	^ self mergeExpression: '>>' with: aDRJitCompileTimeExpression
]

{ #category : #visiting }
DRJitCompileTimeExpression >> acceptVisitor: aVisitor [

	^ aVisitor visitJITCompileTimeExpression: self
]

{ #category : #'bit manipulation' }
DRJitCompileTimeExpression >> anyMask: aDRJitCompileTimeExpression [

	^ self mergeExpression: 'anyMask:' with: aDRJitCompileTimeExpression
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
DRJitCompileTimeExpression >> isNumberValue [

	^ false
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

{ #category : #'cogit-generation' }
DRJitCompileTimeExpression >> rtlOperandQualifier [

	^ 'Cq:'
]

{ #category : #operations }
DRJitCompileTimeExpression >> sccpEquals: aDRJitCompileTimeExpression [

	^ self mergeExpression: '=' with: aDRJitCompileTimeExpression
]

{ #category : #operations }
DRJitCompileTimeExpression >> sccpLessThanOrEquals: aDRJitCompileTimeExpression [

	^ self mergeExpression: '<=' with: aDRJitCompileTimeExpression
]

{ #category : #accessing }
DRJitCompileTimeExpression >> type [

	^ DRSignedIntegerType size: 8 "bytes per word"
]
