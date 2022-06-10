Class {
	#name : #DRCode,
	#superclass : #DRValue,
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

{ #category : #activation }
DRCode >> pushFrameIn: interpreter receiver: aReceiver [

	| frame newMethodEntry incomingBasicBlock |
	"Store the current execution state before the call.
	We will restore it just after the return"
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

	self codeNode temporaryNames , self codeNode argumentNames do: [ :e | 
		frame defineVariableNamed: e ].
	frame returnValue: DRNullValue new.
	interpreter pushFrame: frame
]
