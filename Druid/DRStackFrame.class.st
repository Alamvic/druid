Class {
	#name : #DRStackFrame,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'method',
		'irGenerator',
		'entryBasicBlock',
		'exitBasicBlock',
		'operandStack',
		'currentTemporaryFrame',
		'executionState',
		'frameState'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRStackFrame >> definedTemporaryNames [
	
	^ method argumentNames, method temporaryNames
]

{ #category : #accessing }
DRStackFrame >> entryBasicBlock [

	^ entryBasicBlock
]

{ #category : #accessing }
DRStackFrame >> entryBasicBlock: anObject [

	entryBasicBlock := anObject
]

{ #category : #accessing }
DRStackFrame >> executionState [
	^ executionState
]

{ #category : #accessing }
DRStackFrame >> executionState: anExecutionState [
	
	executionState := anExecutionState
]

{ #category : #accessing }
DRStackFrame >> exitBasicBlock [
	^ exitBasicBlock
]

{ #category : #accessing }
DRStackFrame >> exitBasicBlock: aDRBasicBlock [
	
	exitBasicBlock := aDRBasicBlock 
]

{ #category : #'execution state' }
DRStackFrame >> frameState [
	
	frameState ifNotNil: [ ^ frameState ].
	^ self freshFrameState
]

{ #category : #accessing }
DRStackFrame >> frameState: aDRFrameState [ 

	frameState := aDRFrameState
]

{ #category : #'execution state' }
DRStackFrame >> freshFrameState [

	| freshState |
	freshState := self newFrameState.
	freshState definePhisWith: [ DRNullValue new ].
	^ freshState
]

{ #category : #accessing }
DRStackFrame >> homeFrame [
	
	^ self
]

{ #category : #accessing }
DRStackFrame >> initialize [

	super initialize.
	operandStack := Stack new.
	currentTemporaryFrame := Dictionary new
]

{ #category : #accessing }
DRStackFrame >> irGenerator [

	^ irGenerator
]

{ #category : #accessing }
DRStackFrame >> irGenerator: anObject [

	irGenerator := anObject
]

{ #category : #accessing }
DRStackFrame >> method [

	^ method
]

{ #category : #accessing }
DRStackFrame >> method: anObject [

	method := anObject
]

{ #category : #'execution state' }
DRStackFrame >> newFrameState [

	^ DRFrameState new
		  frame: self;
		  builder: irGenerator;
		  yourself
]

{ #category : #accessing }
DRStackFrame >> operandStack [

	^ operandStack
]

{ #category : #accessing }
DRStackFrame >> operandStack: anObject [

	operandStack := anObject
]

{ #category : #accessing }
DRStackFrame >> pop [

	^ operandStack pop
]

{ #category : #printing }
DRStackFrame >> printOn: stream [

	super printOn: stream.
	stream
		nextPutAll: '(';
		print: method selector;
		nextPutAll: ')'
]

{ #category : #accessing }
DRStackFrame >> push: aValue [

	operandStack push: aValue
]

{ #category : #accessing }
DRStackFrame >> receiver [
	^ receiver
]

{ #category : #accessing }
DRStackFrame >> receiver: aCogVMSimulatorLSB [ 
	receiver := aCogVMSimulatorLSB
]

{ #category : #accessing }
DRStackFrame >> returnValue [

	^ currentTemporaryFrame at: '__return'
]

{ #category : #accessing }
DRStackFrame >> returnValue: aValue from: aBasicBlock [

	"this is always a Phi function"
	| phiFunction |
	phiFunction := self returnValue.
	
	"If the value is a phi function, we have to merge it...
	Implement it later"
	phiFunction addValue: aValue
]

{ #category : #accessing }
DRStackFrame >> stackTop [
	
	^ operandStack top
]

{ #category : #accessing }
DRStackFrame >> temporaryFrame [
	
	^ currentTemporaryFrame
]

{ #category : #accessing }
DRStackFrame >> temporaryFrame: anObject [

	currentTemporaryFrame := anObject
]
