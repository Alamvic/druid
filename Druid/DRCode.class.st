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

{ #category : 'accessing' }
DRCode >> originAST [

	^ self codeNode 
]

{ #category : 'activation' }
DRCode >> pushFrame: frame In: interpreter maybeClosure: maybeClosure [

	| newMethodEntry incomingBasicBlock receiver |
	"Store the current execution state before the call.
	We will restore it just after the return"
	incomingBasicBlock := interpreter currentBasicBlock.
	newMethodEntry := interpreter newBasicBlock.
	incomingBasicBlock jumpTo: newMethodEntry.

	frame
		method: self codeNode;
		irGenerator: interpreter;
		entryBasicBlock: newMethodEntry;
		maybeClosure: maybeClosure;
		receiver: maybeClosure.

	(interpreter temporariesListfrom: self codeNode) do: [ :e |
		frame defineVariableNamed: e ].
	interpreter pushFrame: frame.

	"Initialize the default self for returns!
	Let's not consider it as pushed to the stack"
	(interpreter isKindOf: DRMetaCompilerIRGenerator)
		ifTrue: [
				receiver := interpreter
					            addInstructionFrom: self codeNode
					            instructionKind: DRCGLoadReceiver ]
		ifFalse: [
				receiver := interpreter
					            addInstructionFrom: self codeNode
					            instructionKind: DRBCLoadReceiver
					            operands: { interpreter scope initInstruction } ].
	interpreter popOperand.
	frame returnValue: receiver.

	^ frame
]

{ #category : 'activation' }
DRCode >> pushFrameIn: interpreter maybeClosure: maybeClosure [

	^ self pushFrame: self basicNewFrame In: interpreter maybeClosure: maybeClosure
]
