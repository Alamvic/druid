Class {
	#name : #DRPhiFunction,
	#superclass : #DRValue,
	#instVars : [
		'joinedVariables',
		'order',
		'result',
		'originBlocks'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRPhiFunction class >> joinedVariables: aCollection [ 

	^ self new
		joinedVariables: aCollection;
		yourself
]

{ #category : #visiting }
DRPhiFunction >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitPhiFunction: self
]

{ #category : #adding }
DRPhiFunction >> addValue: aValue from: aDRBasicBlock [ 
	
	joinedVariables add: aValue.
	originBlocks add: aDRBasicBlock
]

{ #category : #accessing }
DRPhiFunction >> initialize [

	super initialize.
	joinedVariables := OrderedCollection new.
	originBlocks := OrderedCollection new.
]

{ #category : #testing }
DRPhiFunction >> isPhiFunction [

	^ true
]

{ #category : #accessing }
DRPhiFunction >> joinedVariables [
	^ joinedVariables
]

{ #category : #accessing }
DRPhiFunction >> joinedVariables: aCollection [
	
	joinedVariables := aCollection
]

{ #category : #accessing }
DRPhiFunction >> operands [
	
	^ joinedVariables
]

{ #category : #accessing }
DRPhiFunction >> operands: aCollection [ 
	
	self joinedVariables: aCollection
]

{ #category : #accessing }
DRPhiFunction >> order [
	^ order
]

{ #category : #accessing }
DRPhiFunction >> order: anInteger [
	
	order := anInteger
]

{ #category : #accessing }
DRPhiFunction >> originBlocks: aCollection [ 
	originBlocks := aCollection
]

{ #category : #accessing }
DRPhiFunction >> result [
	
	^ result ifNil: [ self ]
]

{ #category : #accessing }
DRPhiFunction >> result: aPhisicalRegister [

	result := aPhisicalRegister 
]

{ #category : #'cogit-generation' }
DRPhiFunction >> rtlOperandQualifier [
	
	^ 'R:'
]

{ #category : #enumerating }
DRPhiFunction >> valuesWithOriginDo: aFullBlockClosure [ 
	
	joinedVariables with: originBlocks do: aFullBlockClosure
]
