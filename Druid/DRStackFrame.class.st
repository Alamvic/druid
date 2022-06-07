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
		'temporaries',
		'mirrorFrame'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'non local returns' }
DRStackFrame >> addDeferredReturnState: aDRExecutionState [ 
	
	^ mirrorFrame addDeferredReturnState: aDRExecutionState 
]

{ #category : #merging }
DRStackFrame >> addTo: aDRStackFrame [ 
	
	"Add my values to the argument.
	The argument should have all variables defined as phi functions"
	
	temporaries keysAndValuesDo: [ :k :v |
		(aDRStackFrame readTemporaryNamed: k) addValue: v
	]
]

{ #category : #'non lo' }
DRStackFrame >> deferredMethodReturns [
	
	^ mirrorFrame deferredMethodReturns
]

{ #category : #temporaries }
DRStackFrame >> defineVariableNamed: aName [ 
	
	temporaries at: aName put: DRNullValue new
]

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
DRStackFrame >> exitBasicBlock [
	^ exitBasicBlock
]

{ #category : #accessing }
DRStackFrame >> exitBasicBlock: aDRBasicBlock [
	
	exitBasicBlock := aDRBasicBlock 
]

{ #category : #temporaries }
DRStackFrame >> frameDefining: aName withState: aState [
	
	(temporaries includesKey: aName)
		ifTrue: [ ^ self ].

	^ (self outerFrameWithState: aState)
		frameDefining: aName
		withState: aState
]

{ #category : #testing }
DRStackFrame >> hasTemporaryNamed: aName [

	^ temporaries includesKey: aName
]

{ #category : #accessing }
DRStackFrame >> homeFrame [
	1halt.
	^ self
]

{ #category : #accessing }
DRStackFrame >> homeFrameWithState: aState [
	
	^ self
]

{ #category : #accessing }
DRStackFrame >> initialize [

	super initialize.
	operandStack := Stack new.
	temporaries := Dictionary new.
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

{ #category : #accessing }
DRStackFrame >> mirrorFrame [
	^ mirrorFrame
]

{ #category : #accessing }
DRStackFrame >> mirrorFrame: aDRMirrorFrame [ 
	mirrorFrame := aDRMirrorFrame
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

{ #category : #accessing }
DRStackFrame >> postCopy [

	super postCopy.
	temporaries := temporaries copy.
]

{ #category : #printing }
DRStackFrame >> printOn: stream [

	stream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: method selector;
		nextPutAll: ')'
]

{ #category : #accessing }
DRStackFrame >> push: aValue [

	operandStack push: aValue
]

{ #category : #temporaries }
DRStackFrame >> readTemporaryNamed: aString [ 
	
	^ temporaries at: aString
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
DRStackFrame >> receiverWithState: aState [
	^ receiver
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
DRStackFrame >> temporariesDo: aFullBlockClosure [ 
	
	temporaries valuesDo: aFullBlockClosure
]

{ #category : #temporaries }
DRStackFrame >> temporaryAt: aName put: aValue withState: aState [

	(self frameDefining: aName withState: aState) writeTemporaryNamed: aName withValue: aValue
]

{ #category : #temporaries }
DRStackFrame >> temporaryAt: aName withState: aState [

	^ (self frameDefining: aName withState: aState) readTemporaryNamed: aName
]

{ #category : #accessing }
DRStackFrame >> temporaryFrame [
	
	^ currentTemporaryFrame
]

{ #category : #accessing }
DRStackFrame >> temporaryFrame: anObject [

	currentTemporaryFrame := anObject
]

{ #category : #accessing }
DRStackFrame >> temporaryNames [
	
	^ temporaries keys
]

{ #category : #temporaries }
DRStackFrame >> writeTemporaryNamed: aString withValue: aValue [
	
	^ temporaries at: aString put: aValue
]
