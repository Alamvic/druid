Class {
	#name : #DruidStackFrame,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'temporaryFrame',
		'irGenerator',
		'methodExitBasicBlock',
		'exitBasicBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidStackFrame >> currentBasicBlock [
	
	^ irGenerator currentBasicBlock
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
		yourself).
]

{ #category : #accessing }
DruidStackFrame >> methodExitBasicBlock [

	^ methodExitBasicBlock
]

{ #category : #accessing }
DruidStackFrame >> methodExitBasicBlock: anObject [

	methodExitBasicBlock := anObject
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
	1 haltIf: [ aValue isPhiFunction ].
	phiFunction addValue: aValue from: self currentBasicBlock.
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
