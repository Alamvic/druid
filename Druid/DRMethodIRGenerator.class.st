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

	| newIR |
	newIR := DRMethodIRGeneratorInline new
		         controlFlowGraph: controlFlowGraph;
					typeSystem: typeSystem;
		         yourself.
	newIR executionState: executionState copy.
	^ newIR
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
		result scope: (self ir scope lookupVar: aRBAssignmentNode variable name).
		self topFrame
			temporaryAt: aRBAssignmentNode variable name
			put: result
			withState: executionState.

		^ result ].
	

	self unexplored
]

{ #category : 'interpreting' }
DRMethodIRGenerator >> interpretBlockValueWith: aRBMessageNode [

	| block |
	
	aRBMessageNode receiver acceptVisitor: self.
	block := self popOperand simplify.
	block isDRBlockClosure ifFalse: [
		^ self resolveMessageSend: aRBMessageNode receiver: block ].
	
	
	self interpretCode: block receiver: block
]

{ #category : 'as yet unclassified' }
DRMethodIRGenerator >> interpretCode: aDRMethod receiver: aReceiver [ 
	
	self setupCFGScope: aDRMethod codeNode.
	
	self pushFrameForCode: aDRMethod receiver: aReceiver.
	aDRMethod executeOn: self.
	
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

{ #category : 'as yet unclassified' }
DRMethodIRGenerator >> setupCFGScope: aNode [ 

	self ir scope
		numArgs: aNode arguments size;
		argumentNames: aNode argumentNames;
		addTemps: aNode temporaryNames;
		opalScope: aNode scope;
		node: aNode
]

{ #category : 'accessing' }
DRMethodIRGenerator >> typeSystem: aDRTypeSystem [

	typeSystem := aDRTypeSystem
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

	| loadArg |
	
	aRBVariableNode isTempVariable ifTrue: [
		| store loadTemp|
		aRBVariableNode variable isEscaping ifFalse: [ 
			store := self topFrame
				         temporaryAt: aRBVariableNode name
				         withState: executionState.
		].
	
		loadTemp := self
			  addInstructionFrom: aRBVariableNode
			  instructionKind: DRLoadTemporaryVariable
			  operands: {
					  aRBVariableNode name asDRValue.
					  store }.
		
		loadTemp scope: (self ir scope lookupVar: aRBVariableNode name).
		 ^ loadTemp.
	].
	"Else, it is an argument"

	loadArg := self
		           addInstructionFrom: aRBVariableNode
		           instructionKind: DRLoadArgument
		           operands: { aRBVariableNode variable index asDRValue }.
	loadArg argName: aRBVariableNode name.
	
	
	loadArg scope: (self ir scope lookupVar: aRBVariableNode name).
	 
	^ loadArg
]
