Class {
	#name : #DruidStackFrame,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'temporaryFrame',
		'irGenerator',
		'entryBasicBlock',
		'exitBasicBlock',
		'operandStack'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidStackFrame >> currentBasicBlock [
	
	^ irGenerator currentBasicBlock
]

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
	temporaryFrame := Dictionary new.
	operandStack := Stack new
]

{ #category : #accessing }
DruidStackFrame >> irGenerator [

	^ irGenerator
]

{ #category : #accessing }
DruidStackFrame >> irGenerator: anObject [

	irGenerator := anObject.
	temporaryFrame at: '__return' put: (DRPhiFunction new
		result: irGenerator allocateTemporaryRegister;
		origin: thisContext stack;
		yourself).
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

	^ temporaryFrame at: '__return'
]

{ #category : #accessing }
DruidStackFrame >> returnValue: aValue [

	"this is always a Phi function"
	| phiFunction |
	phiFunction := self returnValue.
	
	"If the value is a phi function, we have to merge it...
	Implement it later"
	phiFunction addValue: aValue from: self currentBasicBlock.
]

{ #category : #accessing }
DruidStackFrame >> stackTop [
	
	^ operandStack top
]

{ #category : #accessing }
DruidStackFrame >> temporaryAt: aString [
	
	^ temporaryFrame at: aString
]

{ #category : #accessing }
DruidStackFrame >> temporaryAt: aString put: aValue [ 
	
	"Store an intermediate interpretation value in a temporary variable"
	^ temporaryFrame at: aString put: aValue
]

{ #category : #accessing }
DruidStackFrame >> temporaryFrame [

	^ temporaryFrame
]

{ #category : #accessing }
DruidStackFrame >> temporaryFrame: aCollection [ 
	temporaryFrame := aCollection
]
