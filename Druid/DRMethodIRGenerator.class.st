Class {
	#name : 'DRMethodIRGenerator',
	#superclass : 'DRIRGenerator',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'initialization' }
DRMethodIRGenerator >> blockClosureGenerator [

	| blockGenerator |
	blockGenerator := DRBlockIRGenerator new.
	
	"Review this decision..."
	blockGenerator outerContext: self topFrame copy.
	
	blockGenerator ir scope
		outerScope: self ir scope;
		id: self ir scope id + 1;
		addCopiedVarsFromOuterScope: self ir scope.
		
	^ blockGenerator
]

{ #category : 'visiting' }
DRMethodIRGenerator >> finishCodeInterpretation: lastFrame [

	currentBasicBlock return: lastFrame returnedValue
]

{ #category : 'initialization' }
DRMethodIRGenerator >> initialize [

	super initialize.
	controlFlowGraph scope: DRScope new.

	typeSystem := DRPragmaBasedTypeSystem new
]

{ #category : 'initialization' }
DRMethodIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.

	specialCases at: #sendingMessageTo: put: #ignoreMessageWith:.

]

{ #category : 'inline' }
DRMethodIRGenerator >> inlineGenerator [

	| newIRGenerator |
	newIRGenerator := DRMethodIRGeneratorInline new
		         controlFlowGraph: controlFlowGraph;
					typeSystem: typeSystem;
		         yourself.
	newIRGenerator executionState: executionState copy.
	newIRGenerator ensureScope.
	^ newIRGenerator
]

{ #category : 'interpreting' }
DRMethodIRGenerator >> interpretAssignmentNode: aRBAssignmentNode [

	| value |
	
	value := self visitOperand: aRBAssignmentNode value.
	
	aRBAssignmentNode variable binding isInstanceVariable ifTrue: [
		^ self
			  addInstructionWithNoResultFrom: aRBAssignmentNode
			  instructionKind: DRStoreInstanceVariable
			  operands: {
					  self receiver.
					  aRBAssignmentNode variable binding index asDRValue.
					  value } ].

	aRBAssignmentNode variable binding originalVar isTempVariable ifTrue: [
		| result |
		result := self
			          addInstructionWithNoResultFrom: aRBAssignmentNode
			          instructionKind: DRStoreTemporaryVariable
			          operands: {
					          aRBAssignmentNode variable name asDRValue. "Fix temporary names collisions"
					          value }.

		result scope: (self scope lookupVarOrAddNewTemp: aRBAssignmentNode variable name).
		self topFrame
			temporaryAt: aRBAssignmentNode variable name
			put: result
			withState: executionState.

		^ result ].
	

	self unexplored
]

{ #category : 'as yet unclassified' }
DRMethodIRGenerator >> interpretCode: aDRMethod receiver: aReceiver [ 
	
	self setupCFGScope: aDRMethod codeNode.
	
	self pushFrameForCode: aDRMethod receiver: aReceiver.
	aDRMethod executeOn: self.
	
	^ self popFrame.
]

{ #category : 'as yet unclassified' }
DRMethodIRGenerator >> interpretCode: aDRCode receiver: aReceiver arguments: arguments [
	
	self setupCFGScope: aDRCode codeNode.
	
	self pushFrameForCode: aDRCode receiver: aReceiver.
	
	"Iterate based on the defined arguments.
	In ifNotNil: arguments are optional"
	1 to: aDRCode codeNode argumentNames size do: [ :i | | name |
		name := aDRCode codeNode argumentNames at: i.
		self topFrame writeTemporaryNamed: name withValue: (arguments at: i).
	].
	
	aDRCode executeOn: self.
	^ self popFrame.
]

{ #category : 'initialization' }
DRMethodIRGenerator >> newCFG [
	
	^ DRMethodControlFlowGraph new
]

{ #category : 'accessing' }
DRMethodIRGenerator >> numberOfArguments: aValue [ 
	
	super numberOfArguments: aValue.
"	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self currentBasicBlock loadArgument: i) ]"
]

{ #category : 'frame-access' }
DRMethodIRGenerator >> popFrameMergingDeferredReturns [

	self resolveDeferredMethodReturns: self topFrame.
	^ executionState popFrame
]

{ #category : 'configure' }
DRMethodIRGenerator >> receiver: aDRValue [

	self push: aDRValue
]

{ #category : 'deferred returns' }
DRMethodIRGenerator >> resolveDeferredMethodReturns: aDRFrame [

	| mirrorFrame newFrameReturn deferredReturnsBlock pieces deferredFrameReturn newReturn exitBasicBlock |
	aDRFrame deferredMethodReturns ifEmpty: [ ^ self ].

	mirrorFrame := aDRFrame mirrorFrame.

	"If there were returns attached to popped frame, merge all of them in a single state. If there was only one return this is important also to override implicit returns"
	exitBasicBlock := self newBasicBlock.
	mirrorFrame poppedValue ifNil: [ "There are no provious returns"
		newFrameReturn := self mergeDeferredReturns: aDRFrame deferredMethodReturns in: exitBasicBlock.
		mirrorFrame poppedValue: newFrameReturn. "Save it in a secure and common place"
		aDRFrame clearDeferredMethodReturns.
		^ self ].

	"There is a previous return, we have to integrate new ones"

	self assert: aDRFrame returnValue isFrameReturn not.
	self assert: mirrorFrame poppedValue isNotNil.

	deferredReturnsBlock := self newBasicBlock.
	deferredFrameReturn := self mergeDeferredReturns: aDRFrame deferredMethodReturns in: deferredReturnsBlock.
	pieces := mirrorFrame poppedValue breakBasicBlock.

	"Update exitBasicBlock and continue from there"
	self currentBasicBlock: pieces last.

	deferredReturnsBlock jumpTo: pieces last.
	newReturn := pieces last addInstruction: (self instructionFactory phiWithVariables: {  }).
	aDRFrame returnValue: newReturn.
	newFrameReturn := self addFrameReturn: aDRFrame.
	mirrorFrame poppedValue replaceUsesBy: newFrameReturn.
	newReturn operands: { 
			mirrorFrame poppedValue.
			deferredFrameReturn }.
	mirrorFrame poppedValue: (newFrameReturn).
	aDRFrame clearDeferredMethodReturns
]

{ #category : 'visiting' }
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	^ self
		  messageSendInstructionFor: aRBMessageNode
		  receiver: receiver
		  arguments: arguments
		  method: nil "Infer methods with a Type System"
]

{ #category : 'accessing' }
DRMethodIRGenerator >> scope [

	^ self ir scope
]

{ #category : 'as yet unclassified' }
DRMethodIRGenerator >> setupCFGScope: aNode [ 
	
	self ir scope
		numArgs: aNode arguments size;
		addArguments: aNode argumentNames;
		addTemps: aNode temporaryNames;
		opalScope: aNode scope;
		node: aNode
]

{ #category : 'accessing' }
DRMethodIRGenerator >> typeSystem: aDRTypeSystem [

	typeSystem := aDRTypeSystem
]

{ #category : 'accessing' }
DRMethodIRGenerator >> variableFrame [

	^ self topFrame 
]

{ #category : 'accessing' }
DRMethodIRGenerator >> visitArgumentVariableNode: aRBVariableNode [

	| loadArg frame varName |
	varName := aRBVariableNode name.
	frame := self variableFrame.
	((frame hasTemporaryNamed: varName) and: [
		 (frame readTemporaryNamed: varName) isNullValue not ]) ifTrue: [
		| value |
		value := frame readTemporaryNamed: varName.
		value isCopy ifTrue: [
			^ self
				  addInstructionFrom: value
				  instructionKind: DRCopy
				  operands: { value operand1 } ].
		^ self pushOperand: value ].
	
	loadArg := self
		           addInstructionFrom: aRBVariableNode
		           instructionKind: DRLoadArgument
		           operands: { aRBVariableNode variable index asDRValue }.

	loadArg argName: aRBVariableNode name.
	loadArg scope: (self scope lookupVarOrAddNewTemp: aRBVariableNode name).
	 
	^ loadArg

]

{ #category : 'visiting' }
DRMethodIRGenerator >> visitGlobalVariableNode: aRBVariableNode [

	self pushOperand: aRBVariableNode binding asDRValue
]

{ #category : 'visiting' }
DRMethodIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadInstanceVariable
		operands: { 
				self topFrame receiver.
				aRBVariableNode variable index asDRValue }
]

{ #category : 'visiting' }
DRMethodIRGenerator >> visitTemporaryVariableNode: aRBVariableNode [

	| store loadTemp |
	aRBVariableNode variable isEscaping ifFalse: [
		store := self topFrame
			         temporaryAt: aRBVariableNode name
			         withState: executionState ].
	"		store ifNil: [ 1halt ].
		store isStore ifFalse: [ 1halt ]."
	loadTemp := self
		            addInstructionFrom: aRBVariableNode
		            instructionKind: DRLoadTemporaryVariable
		            operands: {
				            aRBVariableNode name asDRValue.
				            store }.


	loadTemp scope:
		(self scope lookupVarOrAddNewTemp: aRBVariableNode name).
	^ loadTemp
]
