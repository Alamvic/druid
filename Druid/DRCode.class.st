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
DRCode >> defaultReturnValue: receiver [

	^ self subclassResponsibility
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

{ #category : 'activation' }
DRCode >> pushFrame: frame In: interpreter receiver: aReceiver [

	| newMethodEntry incomingBasicBlock |
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
	
	self codeNode argumentNames , self codeNode temporaryNames  do: [ :e | frame defineVariableNamed: e ].
	frame returnValue: (self defaultReturnValue: aReceiver).
	interpreter pushFrame: frame.
	^ frame
]

{ #category : 'activation' }
DRCode >> pushFrameIn: interpreter receiver: aReceiver [

	^ self pushFrame: self basicNewFrame In: interpreter receiver: aReceiver
]
