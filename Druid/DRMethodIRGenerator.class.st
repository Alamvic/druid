Class {
	#name : #DRMethodIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRMethodIRGenerator >> finishCodeInterpretation: lastFrame [

	currentBasicBlock return: lastFrame returnedValue
]

{ #category : #initialization }
DRMethodIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.

	specialCases at: #sendingMessageTo: put: #ignoreMessageWith:.

]

{ #category : #inline }
DRMethodIRGenerator >> inlineGenerator [

	| newIR |
	newIR := DRMethodIRGeneratorInline new
		         controlFlowGraph: controlFlowGraph;
		         yourself.
	newIR executionState: executionState copy.
	^ newIR
]

{ #category : #interpreting }
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
		^ self
			  addInstructionWithNoResultFrom: aRBAssignmentNode
			  instructionKind: DRStoreTemporaryVariable
			  operands: { 
					  aRBAssignmentNode variable name asDRValue. "Fix temporary names collisions"
					  value } ].

	self unexplored
]

{ #category : #accessing }
DRMethodIRGenerator >> numberOfArguments: aValue [ 
	
	super numberOfArguments: aValue.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self currentBasicBlock loadArgument: i) ]
]

{ #category : #'frame-access' }
DRMethodIRGenerator >> popFrameMergingDeferredReturns [

	self resolveDeferredMethodReturns: self topFrame.
	^ executionState popFrame
]

{ #category : #configure }
DRMethodIRGenerator >> receiver: aDRValue [

	self push: aDRValue
]

{ #category : #'deferred returns' }
DRMethodIRGenerator >> resolveDeferredMethodReturns: aDRFrame [

	| mirrorFrame newFrameReturn deferredReturnsBlock pieces deferredFrameReturn newReturn |
	aDRFrame deferredMethodReturns ifEmpty: [ ^ self ].
	mirrorFrame := aDRFrame mirrorFrame.

	"If there were returns attached to popped frame, merge all of them in a single state. If there was only one return this is important also to override implicit returns"

	mirrorFrame poppedValue ifNil: [ "There are no provious returns"
		self assert: aDRFrame exitBasicBlock isEmptyBasicBlock.
		newFrameReturn := self mergeDeferredReturns: aDRFrame deferredMethodReturns in: aDRFrame exitBasicBlock.
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
	aDRFrame exitBasicBlock: pieces last.
	self currentBasicBlock: aDRFrame exitBasicBlock.

	deferredReturnsBlock jumpTo: aDRFrame exitBasicBlock.
	newReturn := aDRFrame exitBasicBlock addInstruction: (self instructionFactory phiWithVariables: {  }).
	aDRFrame returnValue: newReturn.
	newFrameReturn := self addFrameReturn: aDRFrame.
	mirrorFrame poppedValue replaceUsesBy: newFrameReturn.
	newReturn operands: { 
			mirrorFrame poppedValue.
			deferredFrameReturn }.
	mirrorFrame poppedValue: (newFrameReturn).
	aDRFrame clearDeferredMethodReturns
]

{ #category : #visiting }
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	^ self
		  messageSendInstructionFor: aRBMessageNode
		  receiver: receiver
		  arguments: arguments
		  method: nil "Infer methods with a Type System"
]

{ #category : #accessing }
DRMethodIRGenerator >> typeSystem [

	^ DRPragmaBasedTypeSystem new
]

{ #category : #visiting }
DRMethodIRGenerator >> visitGlobalVariableNode: aRBVariableNode [

	self flag: #notImplemented.
	self pushOperand: nil
]

{ #category : #visiting }
DRMethodIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadInstanceVariable
		operands: { 
				self topFrame receiver.
				aRBVariableNode name asDRValue }
]

{ #category : #visiting }
DRMethodIRGenerator >> visitTemporaryVariableNode: aRBVariableNode [

	| value |
	value := self temporaryAt: aRBVariableNode name.

	value isNullValue ifTrue: [ "If Null then it is a method local variable"
		^ self
			  addInstructionFrom: aRBVariableNode
			  instructionKind: DRLoadTemporaryVariable
			  operands: { aRBVariableNode name asDRValue } ].

	"Else, it is an argument"
	^ self pushOperand: value
]
