Class {
	#name : #DruidStackFrame,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'irGenerator',
		'entryBasicBlock',
		'exitBasicBlock',
		'operandStack',
		'currentTemporaryFrame'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidStackFrame >> entryBasicBlock [

	^ entryBasicBlock
]

{ #category : #accessing }
DruidStackFrame >> entryBasicBlock: anObject [

	entryBasicBlock := anObject
]

{ #category : #accessing }
DruidStackFrame >> exitBasicBlock [
	^ exitBasicBlock
]

{ #category : #accessing }
DruidStackFrame >> exitBasicBlock: aDRBasicBlock [
	
	exitBasicBlock := aDRBasicBlock 
]

{ #category : #accessing }
DruidStackFrame >> initialize [

	super initialize.
	operandStack := Stack new.
	currentTemporaryFrame := Dictionary new
]

{ #category : #accessing }
DruidStackFrame >> irGenerator [

	^ irGenerator
]

{ #category : #accessing }
DruidStackFrame >> irGenerator: anObject [

	irGenerator := anObject
]

{ #category : #accessing }
DruidStackFrame >> operandStack [

	^ operandStack
]

{ #category : #accessing }
DruidStackFrame >> operandStack: anObject [

	operandStack := anObject
]

{ #category : #accessing }
DruidStackFrame >> pop [

	^ operandStack pop
]

{ #category : #accessing }
DruidStackFrame >> push: aValue [

	operandStack push: aValue
]

{ #category : #accessing }
DruidStackFrame >> receiver [
	^ receiver
]

{ #category : #accessing }
DruidStackFrame >> receiver: aCogVMSimulatorLSB [ 
	receiver := aCogVMSimulatorLSB
]

{ #category : #accessing }
DruidStackFrame >> returnValue [

	^ currentTemporaryFrame at: '__return'
]

{ #category : #accessing }
DruidStackFrame >> returnValue: aValue [

	"this is always a Phi function"
	| phiFunction |
	phiFunction := self returnValue.
	
	"If the value is a phi function, we have to merge it...
	Implement it later"
	phiFunction addValue: aValue.
]

{ #category : #accessing }
DruidStackFrame >> returnValue: aValue from: aBasicBlock [

	"this is always a Phi function"
	| phiFunction |
	phiFunction := self returnValue.
	
	"If the value is a phi function, we have to merge it...
	Implement it later"
	phiFunction addValue: aValue
]

{ #category : #accessing }
DruidStackFrame >> stackTop [
	
	^ operandStack top
]

{ #category : #accessing }
DruidStackFrame >> temporaryFrame [
	
	^ currentTemporaryFrame
]

{ #category : #accessing }
DruidStackFrame >> temporaryFrame: anObject [

	currentTemporaryFrame := anObject
]
