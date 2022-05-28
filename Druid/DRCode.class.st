Class {
	#name : #DRCode,
	#superclass : #DRValue,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #activation }
DRCode >> basicNewFrame [
	
	self subclassResponsibility
]

{ #category : #accessing }
DRCode >> codeNode [

	self subclassResponsibility
]

{ #category : #activation }
DRCode >> pushFrameIn: interpreter receiver: aReceiver [ 
	| frame newMethodEntry incomingBasicBlock newExecutionState |	

	"Store the current execution state before the call.
	We will restore it just after the return"
	interpreter currentFrame executionState: interpreter currentBasicBlock executionState.
	
	incomingBasicBlock := interpreter currentBasicBlock.
	newMethodEntry := interpreter newBasicBlock.
	incomingBasicBlock jumpTo: newMethodEntry.
	
	frame := self basicNewFrame
		receiver: aReceiver;
		method: self codeNode;
		irGenerator: interpreter;
		entryBasicBlock: newMethodEntry;
		exitBasicBlock: interpreter basicNewBasicBlock;
		yourself.


	"Copy the execution state but reset the temporary frame for this new frame"
	newExecutionState := newMethodEntry executionState copy.
	newExecutionState frameState: frame freshFrameState.
	newMethodEntry executionState: newExecutionState.
	frame executionState: newExecutionState.
	
	self codeNode temporaryNames, self codeNode argumentNames
		do: [ :e | frame frameState defineVariableNamed: e ].
	
	interpreter pushFrame: frame.
	frame temporaryFrame
		at: '__return'
		put: (DRPhiFunction new
			result: interpreter allocateTemporaryRegister;
			origin: thisContext stack;
			yourself).
]
