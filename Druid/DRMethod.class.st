Class {
	#name : #DRMethod,
	#superclass : #Object,
	#instVars : [
		'methodNode'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethod class >> methodNode: aNode [

	^ self new
		methodNode: aNode;
		yourself
]

{ #category : #accessing }
DRMethod >> codeNode [
	
	^ self methodNode
]

{ #category : #executing }
DRMethod >> executeOn: interpreter [ 

	self codeNode acceptVisitor: interpreter
]

{ #category : #accessing }
DRMethod >> methodNode [

	^ methodNode
]

{ #category : #accessing }
DRMethod >> methodNode: anObject [

	methodNode := anObject
]

{ #category : #activation }
DRMethod >> pushFrameIn: interpreter receiver: aReceiver [ 
	| frame newMethodEntry incomingBasicBlock |	

	incomingBasicBlock := interpreter currentBasicBlock.
	newMethodEntry := interpreter newBasicBlock.
	incomingBasicBlock jumpTo: newMethodEntry.
	
	frame := DruidStackFrame new
		receiver: aReceiver;
		method: self methodNode;
		irGenerator: interpreter;
		entryBasicBlock: newMethodEntry;
		exitBasicBlock: interpreter basicNewBasicBlock;
		returnValue: (DRPhiFunction new
			result: interpreter allocateTemporaryRegister;
			origin: thisContext stack;
			yourself);
		yourself.

	newMethodEntry executionState temporaryFrame: Dictionary new.
	interpreter pushFrame: frame.
]
