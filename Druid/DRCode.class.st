Class {
	#name : 'DRCode',
	#superclass : 'DRInstruction',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'activation' }
DRCode >> basicNewFrame [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
DRCode >> codeNode [

	self subclassResponsibility
]

{ #category : 'activation' }
DRCode >> defaultReturnValueForInterpreter: interpreter receiver: aReceiver [

	self subclassResponsibility
]

{ #category : 'testing' }
DRCode >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : 'testing' }
DRCode >> isMandatoryInstruction [

	^ true
]

{ #category : 'accessing' }
DRCode >> originAST [

	^ self codeNode 
]

{ #category : 'activation' }
DRCode >> pushFrame: frame In: interpreter receiver: aReceiver [

	| newMethodEntry incomingBasicBlock receiver |
	"Store the current execution state before the call.
	We will restore it just after the return"
	incomingBasicBlock := interpreter currentBasicBlock.
	newMethodEntry := interpreter newBasicBlock.
	incomingBasicBlock jumpTo: newMethodEntry.

	frame
		receiver: aReceiver;
		method: self codeNode;
		irGenerator: interpreter;
		entryBasicBlock: newMethodEntry.
	self setMaybeClosureTo: frame.

	interpreter defineTemporariesFrom: self on: frame.
	interpreter pushFrame: frame.

	"Initialize the default self for returns!
	Let's not consider it as pushed to the stack"
	receiver := self defaultReturnValueForInterpreter: interpreter receiver: aReceiver.
	frame returnValue: receiver.

	^ frame
]

{ #category : 'activation' }
DRCode >> pushFrameIn: interpreter receiver: receiver [

	^ self
		  pushFrame: self basicNewFrame
		  In: interpreter
		  receiver: receiver
]

{ #category : 'initialization' }
DRCode >> setMaybeClosureTo: aFrame [

	self subclassResponsibility
]
