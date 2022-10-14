Class {
	#name : #DRMetaCompilerIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #factory }
DRMetaCompilerIRGenerator >> createInitialBasicBlock [
	| initialBasicBlock |
	initialBasicBlock := super createInitialBasicBlock.
	self currentPrimitiveFailureCode: (initialBasicBlock copy: 0).
	^ initialBasicBlock
]

{ #category : #accessing }
DRMetaCompilerIRGenerator >> currentPrimitiveFailureCode: anInstruction [
	
	self executionState primitiveFailureCode: anInstruction
]

{ #category : #initialization }
DRMetaCompilerIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	
	"Stack access"
	specialCases at: #stackTop put: #interpretStackTopWith:.
	specialCases at: #stackValue: put: #interpretStackValueWith:.
	specialCases at: #pop:thenPush: put: #interpretPopThenPushWith:.

]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> methodFor: aRBMessageNode withReceiver: receiver [

	^ aRBMessageNode isSuperSend
		  ifTrue: [ 
			  self currentMethod methodClass superclass lookupSelector:
				  aRBMessageNode selector ]
		  ifFalse: [ receiver type lookupSelector: aRBMessageNode selector ]
]

{ #category : #accessing }
DRMetaCompilerIRGenerator >> numberOfArguments: aValue [ 
	
	super numberOfArguments: aValue.
	self push: self receiverRegister.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]
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
