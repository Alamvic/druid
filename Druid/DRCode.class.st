Class {
	#name : #DRCode,
	#superclass : #DRInstruction,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #activation }
DRCode >> basicNewFrame [

	^ self subclassResponsibility
]

{ #category : #accessing }
DRCode >> codeNode [

	self subclassResponsibility
]

{ #category : #testing }
DRCode >> isMandatoryInstruction [

	^ true
]

{ #category : #activation }
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
		entryBasicBlock: newMethodEntry;
		exitBasicBlock: interpreter basicNewBasicBlock.

	self codeNode temporaryNames , self codeNode argumentNames do: [ :e | 
		frame defineVariableNamed: e ].
	frame returnValue: DRNullValue new.
	interpreter pushFrame: frame
]

{ #category : #activation }
DRCode >> pushFrameIn: interpreter receiver: aReceiver [

	self
		pushFrame: self basicNewFrame
		In: interpreter
		receiver: aReceiver
]
