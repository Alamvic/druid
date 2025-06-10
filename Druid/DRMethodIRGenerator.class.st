Class {
	#name : 'DRMethodIRGenerator',
	#superclass : 'DRIRGenerator',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'visiting' }
DRMethodIRGenerator >> finishCodeInterpretation: lastFrame [

	currentBasicBlock return: lastFrame returnedValue
]

{ #category : 'initialization' }
DRMethodIRGenerator >> initialize [

	super initialize.

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
		result vectorName: (self ir scope lookupVar: aRBAssignmentNode variable name).
		self topFrame
			temporaryAt: aRBAssignmentNode variable name
			put: result
			withState: executionState.

		^ result ].
	

	self unexplored
]

{ #category : 'as yet unclassified' }
DRMethodIRGenerator >> interpretCode: aDRMethod receiver: aReceiver [ 
	
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

{ #category : 'accessing' }
DRMethodIRGenerator >> typeSystem: aDRTypeSystem [

	typeSystem := aDRTypeSystem
]

{ #category : 'visiting' }
DRMethodIRGenerator >> visitBlockNode: aRBBlockNode [

	| irBlockGenerator blockScope |
	
	irBlockGenerator := self blockClosureGenerator.
	blockScope := DRScope new
		              numArgs: aRBBlockNode arguments size;
		              argumentNames: aRBBlockNode argumentNames;
		              addTemps: aRBBlockNode temporaryNames;
		              opalScope: aRBBlockNode scope;
		              node: aRBBlockNode;
		              id: self ir scope id + 1;
		              outerScope: self ir scope;
		              addCopiedVarsFromOuterScope: self ir scope;
		              yourself.
	irBlockGenerator ir scope: blockScope.

	self pushOperand: (DRBlockClosure new
			 blockNode: aRBBlockNode;
			 irGenerator: irBlockGenerator;
			 outerContext: self topFrame;
			 yourself)
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
DRMethodIRGenerator >> visitMethodNode: aRBMethodNode [

	| methodScope |
	(aRBMethodNode hasPragmaNamed: #druidExitPoint) ifTrue: [
		^ self interpretDruidExitPointWith: aRBMethodNode ].

	(aRBMethodNode hasPragmaNamed: #doNotGenerate) ifTrue: [
		(aRBMethodNode hasPragmaNamed: #druidNotIgnore) ifFalse: [ 1 halt ] ].
	(aRBMethodNode hasPragmaNamed: #halt) ifTrue: [ 1 halt ].
	"(aRBMethodNode hasPragmaNamed: #primitive:) ifTrue: [ 1 halt ]."


	(aRBMethodNode hasPragmaNamed: #druidJITCompileTimeExpression)
		ifTrue: [
			| copy |
			"Only support single return statements for now"
			self assert: aRBMethodNode statements size = 1.
			self assert: aRBMethodNode statements first isReturn.
			copy := self currentBasicBlock copy:
				        (DRInterpreterReference reference:
					         aRBMethodNode statements first value formattedCode).
			self topFrame returnValue: copy.
			self addDeferredReturnState: {
					executionState copy.
					(self addFrameReturn: self topFrame) }.
			^ self ].

	
	methodScope := DRScope new
		               numArgs: aRBMethodNode arguments size;
		               argumentNames: aRBMethodNode argumentNames ;
		               addTemps: aRBMethodNode temporaryNames;
		               opalScope: aRBMethodNode scope;
		               node: aRBMethodNode;
		               yourself.
	self ir scope: methodScope.

	aRBMethodNode statements do: [ :each |
		self assert: self topFrame operandStack isEmpty.
		each acceptVisitor: self.
		self popOperand.
		self assert: self topFrame operandStack isEmpty.
		each isReturn ifTrue: [ ^ self ] ].

	self assert: self topFrame operandStack isEmpty.
	"If the method has no return statement, it should return default value"
	self pushOperand: self topFrame returnValue.
	self addDeferredReturnState: {
			executionState copy.
			(self addFrameReturn: self topFrame) }
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
		
		loadTemp vectorName: (self ir scope lookupVar: aRBVariableNode name).
		 ^ loadTemp.
	].
	"Else, it is an argument"

	loadArg := self
		           addInstructionFrom: aRBVariableNode
		           instructionKind: DRLoadArgument
		           operands: { aRBVariableNode variable index asDRValue }.
	loadArg argName: aRBVariableNode name.
	
	
	loadArg vectorName: (self ir scope lookupVar: aRBVariableNode name).
	 
	^ loadArg
]
