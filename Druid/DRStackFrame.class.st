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
		'temporaries',
		'mirrorFrame',
		'returnValue'
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
	].
	operandStack withIndexDo: [ :e :i |
		(aDRStackFrame operandStack at: i) addValue: e.
	].
	aDRStackFrame returnValue addValue: self returnValue
]

{ #category : #'deferred returns' }
DRStackFrame >> clearDeferredMethodReturns [

	mirrorFrame deferredMethodReturns: Dictionary new
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

	"The home frame of a frame is the method frame where the current frame is defined.
	The home of a method frame is itself.
	The home of a block closure's frame is the method where it was defined.
	To obtain it, a block closure remembers the frame where it was defined (it could be a method frame
	or a closure frame if it is a nested closure), and then we lookup the chain of frames up to a
	method frame."

	^ self
]

{ #category : #accessing }
DRStackFrame >> homeFrameWithState: aState [
	
	^ self
]

{ #category : #accessing }
DRStackFrame >> initialize [

	super initialize.
	mirrorFrame := DRMirrorFrame new.
	operandStack := Stack new.
	temporaries := Dictionary new.
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

{ #category : #copying }
DRStackFrame >> postCopy [

	super postCopy.
	temporaries := temporaries copy.
	operandStack := operandStack copy.
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

{ #category : #'deferred returns' }
DRStackFrame >> resolveDeferredMethodReturns: aDRIRGenerator [

	| returnStates mergedState deferredReturnsBlock newExitBasicBlock |
	self deferredMethodReturns ifEmpty: [ ^ aDRIRGenerator executionState ].

	"If there were returns attached to popped frame, merge all of them in a single state. If there was only one return this is important also to override implicit returns"

	exitBasicBlock isEmptyBasicBlock ifTrue: [ 
		returnStates := self deferredMethodReturns associations collect: [ :assoc | 
			                (exitBasicBlock hasPredecessor: assoc key) ifTrue: [ "Remove to keep the order" 
				                exitBasicBlock removePredecessor: assoc key ].
			                assoc key jumpTo: exitBasicBlock.
			                assoc value popUpTo: self "popAllButTop" ].
		mergedState := DRStackMerger new
			               builder: aDRIRGenerator;
			               mergeBlock: exitBasicBlock;
			               mergeAll: returnStates.
		self clearDeferredMethodReturns.
		self returnValue: mergedState topFrame returnValue.
		^ aDRIRGenerator executionState ].






	deferredReturnsBlock := aDRIRGenerator newBasicBlock.
	self deferredMethodReturns keysDo: [ :pred | pred jumpTo: deferredReturnsBlock ].
	returnStates := self deferredMethodReturns values.
	mergedState := DRStackMerger new
		               builder: aDRIRGenerator;
		               mergeBlock: deferredReturnsBlock;
		               mergeAll: returnStates upTo: self.


	newExitBasicBlock := aDRIRGenerator newBasicBlock.

	exitBasicBlock endInstruction isJump ifTrue: [ 
		| newReturn |
		newExitBasicBlock basicEndInstruction: exitBasicBlock endInstruction.
		exitBasicBlock basicEndInstruction: DRNoop new.
		deferredReturnsBlock jumpTo: newExitBasicBlock.
		exitBasicBlock jumpTo: newExitBasicBlock.
		newReturn := newExitBasicBlock addInstruction:
			             (irGenerator instructionFactory
				              phiWith: mergedState topFrame returnValue
				              with: self returnValue).
		self returnValue: newReturn ].

	exitBasicBlock endInstruction isReturn ifTrue: [ 
		| lastReturn newReturn |
		lastReturn := exitBasicBlock endInstruction operand1. "TODO: Remove, should already be that"
		deferredReturnsBlock jumpTo: newExitBasicBlock.
		exitBasicBlock jumpTo: newExitBasicBlock.
		newReturn := newExitBasicBlock addInstruction:
			             (irGenerator instructionFactory
				              phiWith: mergedState topFrame returnValue
				              with: lastReturn).
				1halt.
		newExitBasicBlock return: newReturn.
		self returnValue: newReturn ].

	self clearDeferredMethodReturns.
	exitBasicBlock := newExitBasicBlock.
"1halt."
	^ aDRIRGenerator executionState
]

{ #category : #accessing }
DRStackFrame >> returnValue [

	^ returnValue
]

{ #category : #accessing }
DRStackFrame >> returnValue: aValue [

	returnValue := aValue
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
DRStackFrame >> temporaryNames [
	
	^ temporaries keys
]

{ #category : #temporaries }
DRStackFrame >> writeTemporaryNamed: aString withValue: aValue [
	
	^ temporaries at: aString put: aValue
]
