Class {
	#name : #DRVMState,
	#superclass : #Object,
	#instVars : [
		'operandStack',
		'primitiveFailureCode',
		'builder'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accumulating }
DRVMState >> addPrimitiveFailureCode: aValue [
	
	primitiveFailureCode addValue: aValue
]

{ #category : #accumulating }
DRVMState >> addStackOperand: operand at: stackPosition [

	| stackOperandPhi |
	"Fill with empty phis if necessary"
	[ stackPosition > operandStack size ] whileTrue: [ 
		operandStack push:
			(builder instructionFactory
				phiWithVariables: OrderedCollection new) ].

	stackOperandPhi := operandStack at: stackPosition.
	"If the operand did not change, do nothing"
	stackOperandPhi == operand ifTrue: [ ^ self ].
	
	"Block closures are special and we do not handle polymorphic blocks.
	If the temporary is a block closure, override the phi function.
	Avoid overriding block closures too"
	stackOperandPhi isDRBlockClosure ifTrue: [ 
		self error: 'cannot override block closures in the stack' ].
	
	operand isDRBlockClosure ifTrue: [
		^ operandStack at: stackPosition put: operand ].
	
	"Otherwise, accumulate it"
	stackOperandPhi addValue: operand
]

{ #category : #accumulating }
DRVMState >> addTo: aDRVMState [ 
	
	"Add my own state to the execution state in parameter"
	aDRVMState completeOperandStackUpTo: operandStack size.
	aDRVMState operandStackWithIndexDo: [ :operand :stackPosition |
		aDRVMState
			addStackOperand: (self stackOperandAt: stackPosition ifAbsent: [ DRNullValue new ])
			at: stackPosition ].
	aDRVMState addPrimitiveFailureCode: primitiveFailureCode.
]

{ #category : #accumulating }
DRVMState >> addTo: aDRVMState depth: maxVMStackDepth [
	
	"Add my own state to the execution state in parameter"

	aDRVMState completeOperandStackUpTo: maxVMStackDepth.
	self completeOperandStackUpTo: maxVMStackDepth do: [ operandStack push: DRNullValue new ].
	aDRVMState operandStackWithIndexDo: [ :operand :stackPosition |
		aDRVMState
			addStackOperand: (self stackOperandAt: stackPosition ifAbsent: [ 1halt. ])
			at: stackPosition ].
	aDRVMState addPrimitiveFailureCode: primitiveFailureCode.
]

{ #category : #merging }
DRVMState >> completeOperandStackUpTo: anInteger [ 
	
	| predecessorsUntilNow |
	predecessorsUntilNow := self maxNumberOfPredecessors.
	self completeOperandStackUpTo: anInteger do: [ 
		operandStack push: (builder instructionFactory
				phiWithVariables: (OrderedCollection
					new: predecessorsUntilNow
					withAll: DRNullValue new)) ]
]

{ #category : #merging }
DRVMState >> completeOperandStackUpTo: anInteger do: aBlockValue [

	| predecessorsUntilNow |
	predecessorsUntilNow := self maxNumberOfPredecessors.
	operandStack size + 1 to: anInteger do: [ :i | aBlockValue cull: i ]
]

{ #category : #initialization }
DRVMState >> initialize [
	super initialize.
	
	operandStack := Stack new
]

{ #category : #initialization }
DRVMState >> initializeOnBuilder: aBuilder [ 
	
	builder := aBuilder.
	primitiveFailureCode := aBuilder instructionFactory phiWithVariables: OrderedCollection new.
	
]

{ #category : #testing }
DRVMState >> isNotSimplified [
	
	"All should be phi functions"
	^ (operandStack allSatisfy: [ :e | 
			e isPhiFunction or: [ e isDRBlockClosure ] ])
				and: [ primitiveFailureCode isPhiFunction  or: [ primitiveFailureCode isDRBlockClosure ] ]
]

{ #category : #accessing }
DRVMState >> maxNumberOfPredecessors [
	
	"Select the max number of operands in the phis"
	| max |
	max := primitiveFailureCode operands size.
	operandStack ifEmpty: [ ^ max ].
	
	^ (operandStack max: [ :e | e operands size ]) max: max
]

{ #category : #accessing }
DRVMState >> operandStack [

	^ operandStack
]

{ #category : #accessing }
DRVMState >> operandStack: anObject [

	operandStack := anObject
]

{ #category : #iterating }
DRVMState >> operandStackWithIndexDo: aFullBlockClosure [ 
	
	operandStack withIndexDo: aFullBlockClosure
]

{ #category : #adding }
DRVMState >> pop [

	operandStack ifEmpty: [ self error: 'Operand stack underflow!' ].
	^ operandStack pop
]

{ #category : #copying }
DRVMState >> postCopy [
	super postCopy.
	operandStack := operandStack copy
]

{ #category : #accessing }
DRVMState >> primitiveFailureCode [

	^ primitiveFailureCode
]

{ #category : #accessing }
DRVMState >> primitiveFailureCode: anObject [

	primitiveFailureCode := anObject
]

{ #category : #adding }
DRVMState >> push: aValue [

	operandStack push: aValue
]

{ #category : #simplification }
DRVMState >> simplify [
	
	| newOperand newPrimitiveFailureCode |
	operandStack withIndexDo: [ :operand :index |
		newOperand := operand simplify.
		newOperand ~~ operand ifTrue: [
			operandStack at: index put: newOperand.
			operand replaceUsesBy: newOperand ] ].

	newPrimitiveFailureCode := primitiveFailureCode simplify.
	newPrimitiveFailureCode ~~ primitiveFailureCode ifTrue: [
		primitiveFailureCode := newPrimitiveFailureCode.
		primitiveFailureCode replaceUsesBy: newPrimitiveFailureCode ].
]

{ #category : #accumulating }
DRVMState >> stackOperandAt: stackPosition ifAbsent: aBlock [

	^ operandStack at: stackPosition ifAbsent: aBlock
]

{ #category : #adding }
DRVMState >> top [

	operandStack ifEmpty: [ self error: 'Operand stack underflow!' ].
	^ operandStack top
]
