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
	"interpreter pushFrameWithReceiver: aReceiver method: self methodNode.
"
	incomingBasicBlock := interpreter currentBasicBlock.
	newMethodEntry := interpreter newBasicBlock.
	incomingBasicBlock jumpTo: newMethodEntry.
	
	frame := DruidStackFrame new
		receiver: aReceiver;
		method: self methodNode;
		irGenerator: self;
		entryBasicBlock: newMethodEntry;
		exitBasicBlock: interpreter basicNewBasicBlock;
		yourself.
	newMethodEntry executionState frameState temporaries: frame temporaryFrame.
	
	interpreter pushFrame: frame.
	frame temporaryFrame
		at: '__return'
		put: (DRPhiFunction new
			result: interpreter allocateTemporaryRegister;
			origin: thisContext stack;
			yourself).
]
