Class {
	#name : #DRPhiFunction,
	#superclass : #DRInstruction,
	#instVars : [
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
	
	operands add: aValue.
	originBlocks add: aDRBasicBlock
]

{ #category : #accessing }
DRPhiFunction >> initialize [

	super initialize.
	operands := OrderedCollection new.
	originBlocks := OrderedCollection new.
]

{ #category : #testing }
DRPhiFunction >> isEmpty [
	
	^ operands isEmpty 
]

{ #category : #testing }
DRPhiFunction >> isPhiFunction [

	^ true
]

{ #category : #accessing }
DRPhiFunction >> joinedVariables [
	^ operands
]

{ #category : #accessing }
DRPhiFunction >> joinedVariables: aCollection [
	
	operands := aCollection
]

{ #category : #accessing }
DRPhiFunction >> name [
	
	^ self result name
]

{ #category : #printing }
DRPhiFunction >> opcode [
	
	^ 'ø'
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

{ #category : #accessing }
DRPhiFunction >> singleChild [

	operands size > 1
		ifTrue: [ self error: 'Cannot simplify a phi function with more than one operand for now' ].
	^ operands first
]

{ #category : #enumerating }
DRPhiFunction >> valuesWithOriginDo: aFullBlockClosure [ 
	
	operands with: originBlocks do: aFullBlockClosure
]
