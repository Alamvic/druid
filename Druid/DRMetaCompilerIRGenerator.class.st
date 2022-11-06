Class {
	#name : #DRMetaCompilerIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #initialization }
DRMetaCompilerIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	
	variableSpecialCases at: #instructionPointer put: #interpretInstructionPointerWith:.
	
	"Stack access"
	specialCases at: #stackTop put: #interpretStackTopWith:.
	specialCases at: #stackValue: put: #interpretStackValueWith:.
	specialCases at: #pop:thenPush: put: #interpretPopThenPushWith:.
	specialCases at: #pop: put: #interpretPopThenPushWith:.
]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> methodFor: aRBMessageNode withReceiver: receiver [

	^ aRBMessageNode isSuperSend
		  ifTrue: [ 
			  self currentMethod methodClass superclass lookupSelector:
				  aRBMessageNode selector ]
		  ifFalse: [ receiver type lookupSelector: aRBMessageNode selector ]
]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	| instruction method |
	method := self methodFor: aRBMessageNode withReceiver: receiver.
	instruction := self
		               instantiate: DRMethod
		               operands: { receiver } , arguments.
	instruction methodNode: method ast.
	self
		interpretCode: instruction
		receiver: receiver
		arguments: arguments
]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	variableSpecialCases at: aRBVariableNode name ifPresent: [ :mapping | 
		^ self perform: mapping withArguments: { aRBVariableNode } ].

	"Resolve it as a variableAccess"
	self pushOperand:
		((self topFrame homeFrameWithState: executionState) receiver value 
			 instVarNamed: aRBVariableNode name) asDRValue
]
