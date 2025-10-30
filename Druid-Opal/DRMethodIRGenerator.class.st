Class {
	#name : 'DRMethodIRGenerator',
	#superclass : 'DRIRGenerator',
	#category : 'Druid-Opal-Compiler',
	#package : 'Druid-Opal',
	#tag : 'Compiler'
}

{ #category : 'building' }
DRMethodIRGenerator >> blockClosureFor: aRBBlockNode [

	^ (super blockClosureFor: aRBBlockNode)
		  irGenerator: self blockClosureGenerator;
		  ensureIR;
		  yourself
]

{ #category : 'building' }
DRMethodIRGenerator >> blockClosureGenerator [

	| blockGenerator |
	blockGenerator := DRBlockIRGenerator new.
	
	"Review this decision..."
	blockGenerator variableFrame: self topFrame copy.
	
	blockGenerator ir scope
		outerScope: self scope;
		id: self scope id + 1.
		
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
DRMethodIRGenerator >> initializeArgumentsNames: aDRCode with: argumentsValues [

	| argumentNames |
	argumentNames := aDRCode codeNode argumentNames.
	1 to: argumentNames size do: [ :i |
		| name |
		name := argumentNames at: i.
		self topFrame
			writeTemporaryNamed: name
			withValue: (argumentsValues at: i) ]
]

{ #category : 'initialization' }
DRMethodIRGenerator >> initializeIR [
	"Initialize scope temps"

	super initializeIR.
	self currentBasicBlock initializeScope: self scope
]

{ #category : 'initialization' }
DRMethodIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.

	specialCases at: #sendingMessageTo: put: #ignoreMessageWith:.

]

{ #category : 'inline' }
DRMethodIRGenerator >> inlineGenerator [

	| newIRGenerator executionStateCopy |
	newIRGenerator := DRMethodIRGeneratorInline new
		                  controlFlowGraph: controlFlowGraph;
		                  typeSystem: typeSystem;
		                  outerScope: self scope;
		                  yourself.

	executionStateCopy := executionState copy.
	newIRGenerator executionState: executionStateCopy.

	newIRGenerator generateScope.
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

	aRBAssignmentNode variable binding isClassVariable ifTrue: [
		^ self
			  addInstructionWithNoResultFrom: aRBAssignmentNode
			  instructionKind: DRStoreLiteralVariable
			  operands: {
					  aRBAssignmentNode variable binding asDRValue.
					  value } ].

	aRBAssignmentNode variable binding originalVar isTempVariable
		ifTrue: [
			| result |
			result := self
				          addInstructionWithNoResultFrom: aRBAssignmentNode
				          instructionKind: DRStoreTemporaryVariable
				          operands: {
						          aRBAssignmentNode variable name asDRValue. "Fix temporary names collisions"
						          value }.

			result scope:
				(self lookupScopeDefiningVariable: aRBAssignmentNode variable).

			(self hasTemporary: aRBAssignmentNode variable name) ifTrue: [
				self topFrame
					temporaryAt: aRBAssignmentNode variable name
					put: result
					withState: executionState ].

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

	self
		initializeArgumentsNames: aDRCode
		with: arguments.

	aDRCode executeOn: self.
	^ self popFrame
]

{ #category : 'as yet unclassified' }
DRMethodIRGenerator >> interpretInlinedBlockBody: aRBBlockNode [
	"We add new temporaries to the current scope"

	self scope addTempVariabless:
		(aRBBlockNode temporaries collect: [ :temp |
			 temp variable originalVar ]).
	self scope addTempVariabless:
		(aRBBlockNode arguments collect: [ :temp | temp variable ]).

	self interpretBlockBody: aRBBlockNode
]

{ #category : 'inlining' }
DRMethodIRGenerator >> interpretInlinedCode: code receiver: receiver arguments: arguments [

	| inlineGenerator lastFrame |
	inlineGenerator := self inlineGenerator
		                   numberOfArguments: arguments size;
		                   yourself.
	
	"Jump to inlining block"
	self currentBasicBlock jumpTo: inlineGenerator newBasicBlock.

	lastFrame := inlineGenerator
		             interpretCode: code
		             receiver: receiver
		             arguments: arguments.

	"Jump back and restore the state"
	inlineGenerator currentBasicBlock jumpTo: self newBasicBlock.
	self executionState: inlineGenerator executionState
]

{ #category : 'interpreting' }
DRMethodIRGenerator >> interpretTemporaryVariable: aRBVariableNode [

	| scope loadTemp |
	scope := self lookupScopeDefiningVariable: aRBVariableNode.
	loadTemp := self
		            addInstructionFrom: aRBVariableNode
		            instructionKind: DRLoadTemporaryVariable
		            operands: { aRBVariableNode name asDRValue }.
	loadTemp scope: scope.
	^ loadTemp
]

{ #category : 'special cases' }
DRMethodIRGenerator >> interpretWhileTrueWith: aRBMessageNode [

	| receiver |
	(aRBMessageNode receiver isBlockNode and: [
		 aRBMessageNode arguments first isBlockNode ]) ifTrue: [
		^ super interpretWhileTrueWith: aRBMessageNode ].
	
	"Resolve it as a normal send"
	receiver := self visitOperand: aRBMessageNode receiver.
	^ self resolveMessageSend: aRBMessageNode receiver: receiver
]

{ #category : 'scope' }
DRMethodIRGenerator >> lookupScopeDefiningVariable: aRBVariableNode [

	^ self scope lookupScopeDefining: aRBVariableNode variable originalVar
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
DRMethodIRGenerator >> setupCFGScope: aCodeNode [ 
	
	self ir scope
		numArgs: aCodeNode arguments size;
		addArgumentVariables: (aCodeNode arguments collect: #variable);
		addTempVariabless: (aCodeNode temporaries collect: [:temp | temp variable originalVar]);
		node: aCodeNode
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

	self flag: #TOCHECK. "Can we use the scope? And avoid to manage the variableFrame?"
	frame := self variableFrame.

	(self hasTemporary: varName) ifTrue: [
		| value |
		value := self temporaryAt: varName.

		"TODO: Avoid Nulls here (do not initialize)"
		value simpleConstantFold asDRValue isNullValue ifFalse: [
			^ self pushOperand: value ] ].

	loadArg := self
		           addInstructionFrom: aRBVariableNode
		           instructionKind: DRLoadArgument
		           operands: { aRBVariableNode variable index asDRValue }.

	loadArg argName: aRBVariableNode name.
	loadArg scope: (self lookupScopeDefiningVariable: aRBVariableNode).
	^ loadArg
]

{ #category : 'visiting' }
DRMethodIRGenerator >> visitArrayNode: aRBArrayNode [ 
	
	| values |
	values := aRBArrayNode children collect: [ :each | self visitOperand: each ].
	self
		addInstructionFrom: aRBArrayNode
		instructionKind: DRArray 
		operands: values asArray
]

{ #category : 'visiting' }
DRMethodIRGenerator >> visitClassVariableNode: aRBVariableNode [

	| value |
	value := aRBVariableNode variable asDRValue.
	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadLiteralVariable 
		operands: { value }
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

	"If the variable is in the current frame, then reuse directly the instruction"
	(self hasTemporary: aRBVariableNode name) ifTrue: [
		store := self topFrame
			         temporaryAt: aRBVariableNode name
			         withState: executionState ].
	
	^ self interpretTemporaryVariable: aRBVariableNode.
	

]

{ #category : 'visiting' }
DRMethodIRGenerator >> visitThisContextNode: aRBVariableNode [

	self visitGlobalVariableNode: aRBVariableNode
]
