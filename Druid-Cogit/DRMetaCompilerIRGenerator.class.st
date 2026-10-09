Class {
	#name : 'DRMetaCompilerIRGenerator',
	#superclass : 'DRIRGenerator',
	#category : 'Druid-Cogit-Metacompiler',
	#package : 'Druid-Cogit',
	#tag : 'Metacompiler'
}

{ #category : 'accessing' }
DRMetaCompilerIRGenerator >> allTemporaryInstructions [

	^ #(  )
]

{ #category : 'factory' }
DRMetaCompilerIRGenerator >> createInitialBasicBlock [
	| initialBasicBlock |
	initialBasicBlock := super createInitialBasicBlock.
	self currentPrimitiveFailureCode: (initialBasicBlock copy: 0).
	^ initialBasicBlock
]

{ #category : 'accessing' }
DRMetaCompilerIRGenerator >> currentPrimitiveFailureCode: anInstruction [
	
	self executionState primitiveFailureCode: anInstruction
]

{ #category : 'initialization' }
DRMetaCompilerIRGenerator >> initialize [

	super initialize.
	typeSystem := DRSlangTypeSystem new.

]

{ #category : 'special cases' }
DRMetaCompilerIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	
	variableSpecialCases at: #instructionPointer put: #interpretInstructionPointerWith:.
	
	"Stack access"
	specialCases at: #stackTop put: #interpretStackTopWith:.
	specialCases at: #stackValue: put: #interpretStackValueWith:.
	specialCases at: #pop:thenPush: put: #interpretPopThenPushWith:.
	specialCases at: #pop: put: #interpretPopThenPushWith:.
	specialCases at: #push: put: #interpretPushWith:.

	specialCases at: #remember: put: #interpretRememberWith:.
	specialCases at: #fixedPermSpaceStart put: #interpretFixedPermSpaceStartWith:.
	
	"Arithmetics for SmallIntegers"
	specialCases at: #sumSmallInteger:withSmallInteger:ifOverflow: put: #interpretSumSmallIntegerWithOverflowWith:.
	specialCases at: #sumSmallInteger:withSmallInteger:ifNotOverflow: put: #interpretSumSmallIntegerWithNoOverflowWith:.
	specialCases at: #subSmallInteger:withSmallInteger:ifOverflow: put: #interpretSubSmallIntegerWithOverflowWith:.
	specialCases at: #multiplySmallInteger:withSmallInteger:ifOverflow: put: #interpretMultiplySmallIntegerWithOverflowWith:.

	"Types"
	specialCases at: #asUnsignedInteger put: #interpretAsUnsignedIntegerWith:.
	specialCases at: #sizeof: put: #interpretSizeofWith:.
	specialCases at: #asFloat put: #interpretAsFloatWith:.
	specialCases at: #rawFloatBitsOf: put: #interpretRawFloatBitsWith:.
	specialCases at: #floatFromRawBits: put: #interpretFloat64FromRawBitsWith:.
	specialCases at: #cCoerceSimple:to: put: #interpretCCoerceWith:.	
	specialCases at: #cCoerce:to: put: #interpretCCoerceWith:.	
	specialCases at: #asObjectReference put: #interpretObjectReferenceCoercionWith:.

	"Memory accesses"
	specialCases at: #longAt: put: #interpretMachineWordAtWith:.
	specialCases at: #byteAt:put: put: #interpretStoreUint8AtWith:.
	self flag: #TODO. "Add types to Druid"
	specialCases at: #uint8At: put: #interpretLoadUint8AtWith:.
	specialCases at: #uint8At:put: put: #interpretStoreUint8AtWith:.
	specialCases at: #uint16At: put: #interpretLoadUint16AtWith:.
	specialCases at: #uint16At:put: put: #interpretStoreUint16AtWith:.
	specialCases at: #uint32At: put: #interpretLoadUint32AtWith:.
	specialCases at: #uint32At:put: put: #interpretStoreUint32AtWith:.
	specialCases at: #uint32AtPointer: put: #interpretLoadUint32AtWith:.
	specialCases at: #uint32AtPointer:put: put: #interpretStoreUint32AtWith:.
	specialCases at: #uint64At: put: #interpretLoadUint64AtWith:.
	specialCases at: #uint64At:put: put: #interpretStoreUint64AtWith:.
	specialCases at: #float64At: put: #interpretLoadFloat64AtWith:.
	specialCases at: #float64At:put: put: #interpretStoreFloat64AtWith:.
	specialCases at: #unsignedLongAt: put: #interpretMachineWordAtWith:.
	specialCases at: #unsignedLong64At: put: #interpretMachineWordAtWith:.
	
	specialCases at: #longAt:put: put: #interpretMachineWordAtPutWith:.
	specialCases at: #unsignedLong64At:put: put: #interpretMachineWordAtPutWith:.
	specialCases at: #unsignedLongAt:put: put: #interpretMachineWordAtPutWith:.
	
	"special simulation cases"
	specialCases at: #cCode: put: #interpretCCodeInSmalltalkWith:.
	specialCases at: #cCode:inSmalltalk: put: #interpretCCodeInSmalltalkWith:.
	
	"Special pharo things such as exceptions"
	specialCases at: #error: put: #ignoreMessageWith:.
	specialCases at: #assert: put: #ignoreMessageWith:.
	specialCases at: #deny: put: #ignoreMessageWith:.
	
	"Interpreter variables"
	variableSpecialCases at: #nilObj put: #interpretNilObjectWith:.
	variableSpecialCases at: #falseObj put: #interpretFalseObjectWith:.
	variableSpecialCases at: #trueObj put: #interpretTrueObjectWith:.

	variableSpecialCases at: #argumentCount put: #interpretArgumentCountWith:.
	variableSpecialCases at: #scavengeThreshold put: #interpretScavengeThresholdWith:.

	variableSpecialCases at: #freeStart put: #interpretFreeStartWith:.
	variableSpecialCases at: #newSpaceStart put: #interpretNewSpaceStartWith:.
	variableSpecialCases at: #spaceMaskToUse put: #interpretSpaceMaskToUseWith:.
	variableSpecialCases at: #newSpaceMask put: #interpretNewSpaceMaskWith:.
	variableSpecialCases at: #oldSpaceMask put: #interpretOldSpaceMaskWith:.
	variableSpecialCases at: #permSpaceMask put: #interpretPermSpaceMaskWith:.
	
	"Border case, this should not be defined here, it should be extensible"
	variableSpecialCases at: #hiddenRootsObj put: #interpretHiddenRootsObjectWith:.
	variableSpecialCases at: #classTableFirstPage put: #interpretClassTableRootsObjectWith:.	
]

{ #category : 'interpreting' }
DRMetaCompilerIRGenerator >> interpretAssignmentNode: aRBAssignmentNode [

	| value |
	aRBAssignmentNode value acceptVisitor: self.
	"Keep instruction in CFG for inlinings"
	value := currentBasicBlock strongCopyForInlining: self operandStackTop. 

	aRBAssignmentNode variable binding isInstanceVariable ifTrue: [
		^ self receiver value
			  instVarNamed: aRBAssignmentNode variable name
			  put: value ].

	aRBAssignmentNode variable binding originalVar isTempVariable
		ifTrue: [
			^ self topFrame
				  temporaryAt: aRBAssignmentNode variable name
				  put: value
				  withState: executionState ].

	self halt
]

{ #category : 'interpretation' }
DRMetaCompilerIRGenerator >> interpretFixedPermSpaceStartWith: aRBMessageNode [

	^ self
		  addCopy: aRBMessageNode
		  with: DRInterpreterReference getFixedPermSpaceStart
]

{ #category : 'special cases' }
DRMetaCompilerIRGenerator >> interpretPushWith: aRBMessageNode [ 
	
	| receiver |
	receiver := self visitOperand: aRBMessageNode arguments first.
	^ self addInstructionWithNoResultFrom: aRBMessageNode instructionKind: DRPush operands: { receiver }
]

{ #category : 'special-cases' }
DRMetaCompilerIRGenerator >> interpretRememberWith: aRBMessageNode [

	| obj |
	obj := self visitOperand: aRBMessageNode arguments first.
	^ (self
		   addInstructionCall: {
				   'ceStoreCheckTrampoline'.
				   obj }
		   from: aRBMessageNode) beMapped saveLinkReg
]

{ #category : 'as yet unclassified' }
DRMetaCompilerIRGenerator >> interpretValue: anOCLiteralValueNode [

	"Nil is interpreted as Null in the VM"
	anOCLiteralValueNode isNodeNil ifTrue: [ ^ DRNullValue new ].
	^ super interpretValue: anOCLiteralValueNode
]

{ #category : 'visiting' }
DRMetaCompilerIRGenerator >> methodFor: aRBMessageNode withReceiver: receiver [

	| methodFound |
	methodFound := super methodFor: aRBMessageNode withReceiver: receiver.
	methodFound ifNil: [
		self error:
			'lookup failed for: ' , aRBMessageNode selector , ' in class '
			, receiver name ].
	^ methodFound
]

{ #category : 'calling convention' }
DRMetaCompilerIRGenerator >> numRegArgs [

	^ executionState baseFrame receiver value cogit numRegArgs
]

{ #category : 'accessing' }
DRMetaCompilerIRGenerator >> numberOfArguments: aValue [ 
	
	super numberOfArguments: aValue.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]
]

{ #category : 'frame-access' }
DRMetaCompilerIRGenerator >> popFrameMergingDeferredReturns [

	| exitBasicBlock topReturns |
	exitBasicBlock := self basicNewBasicBlock.
	topReturns := self topFrame deferredMethodReturns.
	topReturns size > 0
		ifTrue: [
			| mergedState |
			self assert: exitBasicBlock predecessors isEmpty. "If not, check if new predecessors will be added"
			topReturns keysDo: [ :frameReturn | 
				frameReturn breakBasicBlock. "Breaking the block in case of be shared by many returns"
				frameReturn basicBlock jumpTo: exitBasicBlock ].
			mergedState := DRStackMerger new
				               builder: self;
				               mergeBlock: exitBasicBlock;
				               mergeAll: topReturns values upTo: self topFrame.
			executionState := mergedState.
			currentBasicBlock := exitBasicBlock ].

	"Returns installed in the CFG -> stop tracking"
	self topFrame clearDeferredMethodReturns.

	^ executionState popFrame
]

{ #category : 'configure' }
DRMetaCompilerIRGenerator >> receiver: aDRValue [

	| loadReceiver |
	loadReceiver := self 
		instantiate: DRLoadReceiver
		operands: { }.
	loadReceiver origin: thisContext sender stack.
	self currentBasicBlock addInstruction: loadReceiver.
	self push: loadReceiver
]

{ #category : 'visiting' }
DRMetaCompilerIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	| instruction method |
	method := self methodFor: aRBMessageNode withReceiver: receiver.
	
	"If the method has a special ir builder case, use that"
	method ast pragmaNamed: #druidIRBuilder: ifPresent: [ :pragma |
		^ self perform: pragma arguments first value withArguments: { aRBMessageNode } ].
	
	"Otherwise, interpret the method recursively"
	instruction := self
		               instantiate: DRMethod
		               operands: { receiver } , arguments.
	instruction methodNode: method ast.
	self
		interpretCode: instruction
		receiver: receiver
		arguments: arguments
]

{ #category : 'visiting' }
DRMetaCompilerIRGenerator >> trackStackInstruction: aDRStrackInstruction [ 

	aDRStrackInstruction stackDependencies: executionState lastStackInstruction.
	executionState lastStackInstruction: aDRStrackInstruction.
]

{ #category : 'private' }
DRMetaCompilerIRGenerator >> typeOf: aRBVariableNode [ 

	^ typeSystem typeOf: aRBVariableNode
]

{ #category : 'visiting' }
DRMetaCompilerIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	variableSpecialCases at: aRBVariableNode name ifPresent: [ :mapping | 
		^ self perform: mapping withArguments: { aRBVariableNode } ].

	"Resolve it as a variableAccess"
	self pushOperand:
		((self topFrame homeFrameWithState: executionState) receiver simpleConstantFold value 
			 instVarNamed: aRBVariableNode name) asDRValue
]
