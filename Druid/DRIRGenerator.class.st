Class {
	#name : 'DRIRGenerator',
	#superclass : 'Object',
	#instVars : [
		'compiler',
		'specialCases',
		'currentBasicBlock',
		'firstBasicBlock',
		'numberOfArguments',
		'controlFlowGraph',
		'executionState',
		'variableSpecialCases',
		'typeSystem',
		'staging',
		'currentASTNode'
	],
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'visiting' }
DRIRGenerator >> addCopy: aRBVariableNode with: operand [

	^ self addInstructionFrom: aRBVariableNode instructionKind: DRCopy operands: { operand }
]

{ #category : 'execution state' }
DRIRGenerator >> addDeferredReturnState: anExecutionState [

	"Record that this execution stack did a method return
	We will merge this one with other returned states when this method finishes"

	^ (self topFrame homeFrameWithState: anExecutionState first) 
		  addDeferredReturnState: anExecutionState
]

{ #category : 'visiting' }
DRIRGenerator >> addEndInstruction: instruction [

	self currentBasicBlock endInstruction: instruction.
	instruction originAST: currentASTNode
]

{ #category : 'execution state' }
DRIRGenerator >> addFrameReturn: aDRStackFrame [

	self
		addInstructionFrom: aDRStackFrame method
		instructionKind: DRFrameReturn
		operands: { aDRStackFrame returnValue }.
	^ aDRStackFrame returnValue: self popOperand
]

{ #category : 'visiting' }
DRIRGenerator >> addInstruction: instruction from: aNode [

	instruction origin: thisContext sender stack.
	instruction needsStackFlush ifTrue: [
		self currentBasicBlock flushStack ].
	self currentBasicBlock addInstruction: instruction.
	instruction originAST: aNode.
	self pushOperand: instruction.
	instruction isStackInstruction ifTrue: [
		self trackStackInstruction: instruction ].
	^ instruction
]

{ #category : 'adding' }
DRIRGenerator >> addInstructionCall: operands from: aRBMessageNode [

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRCall
		  operands: operands
]

{ #category : 'visiting' }
DRIRGenerator >> addInstructionFrom: aNode instructionKind: instructionClass [

	^ self
		  addInstructionFrom: aNode
		  instructionKind: instructionClass
		  operands: #(  )
]

{ #category : 'visiting' }
DRIRGenerator >> addInstructionFrom: aNode instructionKind: instructionClass operands: operands [
	
	| instruction |
	instruction := self
		instantiate: instructionClass
		operands: operands.
	^ self addInstruction: instruction from: aNode
]

{ #category : 'visiting' }
DRIRGenerator >> addInstructionWithNoResultFrom: aNode instructionKind: instructionClass operands: operands [
	
	| instruction |
	instruction := self
		instantiateNoResultInstruction: instructionClass
		operands: operands.
	^ self addInstruction: instruction from: aNode
]

{ #category : 'factory' }
DRIRGenerator >> allocateTemporaryRegister [
	
	^ self instructionFactory allocateTemporaryRegister
]

{ #category : 'factory' }
DRIRGenerator >> argRegisterNumber: anInteger [

	"TODO: Merge with #interpretStackValueWith: aRBMessageNode withValue: aValue ?"

	| loadArgument |
	numberOfArguments = anInteger ifTrue: [
		^ self instructionFactory loadReceiver classType:
			  (DRSignedIntegerType size: 8) ].

	loadArgument := (self instructionFactory loadArgument: anInteger)
		                classType: (DRSignedIntegerType size: 8).
	loadArgument origin: thisContext sender stack.
	self currentBasicBlock addInstruction: loadArgument.
	^ loadArgument
]

{ #category : 'factory' }
DRIRGenerator >> argumentOffsetForStackValue: stackValue [

	"Arguments are pushed reversed in the stack (first argument is last in the stack).
	For example, for three arguments, the stack has the following shape: 
	
	...
	stack value 2 => arg 0
	stack value 1 => arg 1
	stack value 0 (top) => arg 2
	"

	^ numberOfArguments - stackValue - 1
]

{ #category : 'basic-blocks' }
DRIRGenerator >> basicNewBasicBlock [
	
	^ self basicNewBasicBlockWithState: nil
]

{ #category : 'basic-blocks' }
DRIRGenerator >> basicNewBasicBlockWithState: anExecutionState [

	| block |
	block := controlFlowGraph newBasicBlock.
	block origin: thisContext stack.
	^ block
]

{ #category : 'ir-genera' }
DRIRGenerator >> blockClosureGenerator [

	^ self class new
]

{ #category : 'special cases' }
DRIRGenerator >> branchFrom: startingBasicBlock onEdge: anEdgeBlock doing: aBlockToEvaluate [

	| branchEntryBlock branchExitBlock |	
	branchEntryBlock := self newBasicBlockWithState: nil during: aBlockToEvaluate.
	
	"Then update the successor and predecessor edges"
	anEdgeBlock value: branchEntryBlock.
	branchEntryBlock addPredecessor: startingBasicBlock.
	
	"Get the exit basic block, restore the starting basic block to simplify the caller's interaction"
	branchExitBlock := self currentBasicBlock.
	currentBasicBlock := startingBasicBlock.	
	^ branchExitBlock
]

{ #category : 'building' }
DRIRGenerator >> buildIfTrue: aCondition then: aBuildingBlock [

	| conditionalJump executionStateBeforeBranch trueBranchBasicBlockOut startingBasicBlock |
	conditionalJump := self
		                   instantiateNoResultInstruction:
		                   DRBranchIfCondition
		                   operands: {
				                   DREqualsThanComparison new.
				                   aCondition.
				                   true asDRValue }.
	self addEndInstruction: conditionalJump.

	self pushOperand: DRNullValue new.
	executionStateBeforeBranch := executionState copy.
	self popOperand.

	trueBranchBasicBlockOut := self
		                           branchFrom: self currentBasicBlock
		                           onEdge: [ :branchEntryBlock |
		                           conditionalJump trueBranch:
			                           branchEntryBlock ]
		                           doing: aBuildingBlock.

	"Merge point"
	startingBasicBlock := self currentBasicBlock.
	self newBasicBlock.
	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump falseBranch: self currentBasicBlock.

	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	trueBranchBasicBlockOut hasFinalInstruction
		ifTrue: [ executionState := executionStateBeforeBranch ]
		ifFalse: [
			executionState := DRStackMerger new
				                  builder: self;
				                  mergeBlock: currentBasicBlock;
				                  mergeAll: {
						                  executionStateBeforeBranch.
						                  executionState copy }.
			trueBranchBasicBlockOut jumpTo: self currentBasicBlock ]
]

{ #category : 'accessing' }
DRIRGenerator >> compiler [

	^ compiler
]

{ #category : 'accessing' }
DRIRGenerator >> compiler: anObject [

	compiler := anObject
]

{ #category : 'accessing' }
DRIRGenerator >> compilerOptions [

	^ self compiler compilerOptions
]

{ #category : 'accessing' }
DRIRGenerator >> controlFlowGraph: aDRControlFlowGraph [

	controlFlowGraph := aDRControlFlowGraph
]

{ #category : 'special cases' }
DRIRGenerator >> copyExecutionStateOf: startingBasicBlock [
	
	"Evaluate a block using a copy of the operand stack and temporary frame.
	Copies are needed since branches make divergence points."
	| copy |
	copy := startingBasicBlock executionState copy.
	self topFrame executionState: copy.
	^ copy	
]

{ #category : 'factory' }
DRIRGenerator >> createInitialBasicBlock [

	^ self newBasicBlock
]

{ #category : 'basic-blocks' }
DRIRGenerator >> currentBasicBlock [
	^ currentBasicBlock
]

{ #category : 'lookup' }
DRIRGenerator >> currentMethod [

	^ self topFrame method
]

{ #category : 'frame-access' }
DRIRGenerator >> currentOperandStack [

	^ executionState vmOperandStack
]

{ #category : 'accessing' }
DRIRGenerator >> druidConditionalCompilationWith: aRBMessageNode [

	| condition |
	condition := aRBMessageNode arguments first value.
	^ (self compilerOptions includes: condition)
		ifTrue: [
			| block |
			aRBMessageNode arguments second acceptVisitor: self.
			block := self popOperand simplify.
			block isDRBlockClosure ifFalse: [
				self error: 'Only consider blocks' ].
			block blockNode arguments size > 0 ifTrue: [
				self error: 'Only consider blocks with no arguments' ].
			self interpretCode: block receiver: block arguments: #(  ) ]
		ifFalse: [ self noop: aRBMessageNode ]
]

{ #category : 'execution state' }
DRIRGenerator >> executionStack [
	
	^ executionState executionStack
]

{ #category : 'accessing' }
DRIRGenerator >> executionState [
	
	^ executionState
]

{ #category : 'accessing' }
DRIRGenerator >> executionState: aDRExecutionState [

	executionState := aDRExecutionState
]

{ #category : 'visiting' }
DRIRGenerator >> finishCodeInterpretation: lastFrame [

	self subclassResponsibility
]

{ #category : 'accessing' }
DRIRGenerator >> firstBasicBlock [

	^ firstBasicBlock
]

{ #category : 'stack management' }
DRIRGenerator >> homeFrame [
	
	^ self topFrame homeFrame
]

{ #category : 'accessing' }
DRIRGenerator >> ignoreMessageWith: aRBMessageNode [

	self noop: aRBMessageNode
]

{ #category : 'accessing' }
DRIRGenerator >> initialBasicBlock [
	
	^ controlFlowGraph initialBasicBlock
]

{ #category : 'initialization' }
DRIRGenerator >> initialize [

	| initialExecutionState |
	super initialize.
	controlFlowGraph := self newCFG.
	
	specialCases := Dictionary new.
	variableSpecialCases := Dictionary new.
	self initializeSpecialCases.
	
	"Push a top frame"
	initialExecutionState := DRExecutionStack new.
	
	typeSystem := DRPragmaBasedTypeSystem new.
	
	currentBasicBlock := self initialBasicBlock.
	executionState := DRExecutionState new
		vmState: (DRVMState new
			operandStack: Stack new;
			yourself);
		executionStack: initialExecutionState;
		yourself
]

{ #category : 'initialization' }
DRIRGenerator >> initializeIR [
	"Create the initial basic block"

	| startBlock initialBasicBlock |
	startBlock := self initialBasicBlock.
	initialBasicBlock := self createInitialBasicBlock.
	startBlock jumpTo: initialBasicBlock
]

{ #category : 'special cases' }
DRIRGenerator >> initializeSpecialCases [

	"Control flow and comparisons"
	specialCases at: #druidStageable: put: #interpretStageableBlockWith:.
	specialCases at: #druidIgnore: put: #ignoreMessageWith:.
	specialCases at: #druidIf:do: put: #druidConditionalCompilationWith:.
	specialCases at: #interpreterIgnore: put: #interpretBlockArgumentValueWith:.
	specialCases at: #druidFail put: #interpretDruidFailWith:.

	specialCases at: #value put: #interpretBlockValueWith:.
	specialCases at: #value: put: #interpretBlockValueWith:.
	specialCases at: #cull: put: #interpretBlockCullWith:.
	specialCases at: #ifTrue: put: #interpretIfTrueWith:.
	specialCases at: #whileTrue put: #interpretWhileTrueWith:.
	specialCases at: #whileTrue: put: #interpretWhileTrueWith:.
	specialCases at: #caseOf:otherwise: put: #interpretCaseOfWith:.
	specialCases at: #cppIf:ifTrue: put: #interpretCppIfWith:.
	specialCases at: #cppIf:ifTrue:ifFalse: put: #interpretCppIfWith:.

	specialCases at: #= put: #interpretEqualityComparisonWith:.
	specialCases at: #== put: #interpretEqualityComparisonWith:.
	specialCases at: #~= put: #interpretInequalityComparisonWith:.
	specialCases at: #< put: #interpretLessThanComparisonWith:.
	specialCases at: #<= put: #interpretLessOrEqualsThanComparisonWith:.
	specialCases at: #> put: #interpretGreaterThanComparisonWith:.
	specialCases at: #>= put: #interpretGreaterOrEqualsThanComparisonWith:.
	specialCases at: #| put: #interpretOrWith:.
	specialCases at: #& put: #interpretAndWith:.


	"Arithmetics for Numbers"
	specialCases at: #+ put: #interpretAddWith:.
	specialCases at: #- put: #interpretSubstractWith:.
	specialCases at: #* put: #interpretMultiplyWith:.
	specialCases at: #/ put: #interpretDivisionWith:.
	specialCases at: #// put: #interpretDivisionWith:.
	specialCases at: #\\ put: #interpretModWith:.
	specialCases at: #negated put: #interpretNegatedWith:.
	
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


	"Bit Manipulation"
	specialCases at: #<< put: #interpretLeftShiftWith:.
	specialCases at: #>> put: #interpretRightShiftWith:.
	specialCases at: #>>> put: #interpretArithmeticRightShiftWith:.
	specialCases at: #bitAnd: put: #interpretBitAndWith:.
	specialCases at: #bitOr: put: #interpretBitOrWith:.
	specialCases at: #bitXor: put: #interpretBitXorWith:.
	specialCases at: #anyMask: put: #interpretAnyMaskWith:.
	specialCases at: #rotateLeft:by: put: #interpretRotateLeftBy:.
		
	"special simulation cases"
	specialCases at: #cCode: put: #interpretCCodeInSmalltalkWith:.
	specialCases at: #cCode:inSmalltalk: put: #interpretCCodeInSmalltalkWith:.
	
	"Special pharo things such as exceptions"
	specialCases at: #error: put: #ignoreMessageWith:.
	specialCases at: #assert: put: #ignoreMessageWith:.
	specialCases at: #deny: put: #ignoreMessageWith:.
	
	"special variables"
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

{ #category : 'visiting' }
DRIRGenerator >> inlineGenerator [

	^ self subclassResponsibility
]

{ #category : 'factory' }
DRIRGenerator >> instantiate: anInstructionClass [ 
	
	^ self instantiate: anInstructionClass operands: #()
]

{ #category : 'factory' }
DRIRGenerator >> instantiate: anInstructionClass operands: aCollection [ 
	
	| instruction |
	instruction := anInstructionClass operands: aCollection result: self allocateTemporaryRegister.
	staging = true ifTrue: [ instruction markAsStaged ].
	^ instruction
]

{ #category : 'factory' }
DRIRGenerator >> instantiateNoResultInstruction: anInstructionClass [
	
	^ self instantiateNoResultInstruction: anInstructionClass operands: #()
]

{ #category : 'factory' }
DRIRGenerator >> instantiateNoResultInstruction: anInstructionClass operands: aCollection [ 
	
	| instruction |
	instruction := anInstructionClass operands: aCollection.
	instruction origin: thisContext sender stack.
	instruction result: DRNoRegister new.
	^ instruction
]

{ #category : 'accessing' }
DRIRGenerator >> instructionFactory [
	^ controlFlowGraph instructionFactory
]

{ #category : 'accessing' }
DRIRGenerator >> instructionFactory: aDRInstructionFactory [

	controlFlowGraph instructionFactory: aDRInstructionFactory
]

{ #category : 'interpreting' }
DRIRGenerator >> instructionFor: aDRInstruction inNode: aRBBlockNode [ 
	
	"We need a real instruction (not value), maybe for phi. If it's a value, then copy is created."
	| instruction |
	instruction := aDRInstruction copyToVariableIn: self inNode: aRBBlockNode.
	instruction = aDRInstruction ifFalse: [ self popOperand ]. "If it's a new copy, pop result is needed"
	^instruction
]

{ #category : 'special cases' }
DRIRGenerator >> interpretAddWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRAdd
]

{ #category : 'special cases' }
DRIRGenerator >> interpretAndWith: aRBMessageNode [ 
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRAnd
]

{ #category : 'special cases' }
DRIRGenerator >> interpretAnyMaskWith: aRBMessageNode [ 

	^ self interpretCondition: aRBMessageNode conditionKind: DRTestMask
]

{ #category : 'special cases' }
DRIRGenerator >> interpretArgumentCountWith: aRBVariableNode [

	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRCopy
		  operands: { numberOfArguments asDRValue }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretArithmeticRightShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRSignedRightShift
]

{ #category : 'special cases' }
DRIRGenerator >> interpretAsFloatWith: aRBMessageNode [

	| receiver |
	aRBMessageNode receiver acceptVisitor: self.
	receiver := self popOperand.

	"If it's already a float, do nothing"
	receiver type isFloatType ifTrue: [ ^ self pushOperand: receiver ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRIntToFloat
		  operands: { receiver }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretAsIntegerWith: aRBMessageNode [

	| receiver |
	aRBMessageNode receiver acceptVisitor: self.
	receiver := self popOperand.

	"If it's already an integer, do nothing"
	receiver type isFloatType ifTrue: [ 
		^ self
			  addInstructionFrom: aRBMessageNode
			  instructionKind: DRFloatToInt
			  operands: { receiver } ].
	receiver type isBooleanType ifTrue: [ 
		^ self pushOperand: receiver].

	"Otherwise"
	^ self pushOperand: receiver
]

{ #category : 'special cases' }
DRIRGenerator >> interpretAsUnsignedIntegerWith: aRBMessageNode [ 
	
	^ self interpretUnaryInstruction: aRBMessageNode instructionKind: DRUnsignedCoercion
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretAssignmentNode: aRBAssignmentNode [

	self subclassResponsibility
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBinaryInstruction: aRBMessageNode instructionKind: anInstructionKind [

	| operand1 operand2 |
	operand1 := self visitOperand: aRBMessageNode receiver.
	operand2 := self visitOperand: aRBMessageNode arguments first.

	^ self addInstructionFrom: aRBMessageNode instructionKind: anInstructionKind operands: { operand1 . operand2 }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBitAndWith: aRBMessageNode [
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRBitAnd
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBitOrWith: aRBMessageNode [
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRBitOr
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBitShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRBitShift
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBitXorWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRBitXor
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretBlockActivation: aRBBlockNode [

	| result copy |
	self interpretBlockBody: aRBBlockNode.

	result := self operandStackTop.
	copy := self instructionFor: result inNode: aRBBlockNode.

	"If this is a block activation, it should have a return value"
	self topFrame returnValue: copy
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBlockArgumentValueWith: aRBMessageNode [

	| block |
	aRBMessageNode arguments first acceptVisitor: self.
	block := self popOperand simplify.
	block isDRBlockClosure ifFalse: [ self error: 'Only consider blocks' ].

	block blockNode arguments size > 0 ifTrue: [
		self error: 'Only consider blocks with no arguments' ].
	self interpretCode: block receiver: block arguments: #(  )
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretBlockBody: aRBBlockNode [

	aRBBlockNode statements allButLastDo: [ :e |
		e acceptVisitor: self.
		"If this is a return, exit.
		=> This block returns nothing"
		self popOperand.
		self assert: self topFrame operandStack isEmpty.
		e isReturn ifTrue: [ 
			self pushOperand: DRNullValue new.
			self assert: self topFrame operandStack size = 1.
			^ self ] ].
	
	"Evaluate the last statement but do not pop the operand, as it will be the block's value"
	
	"If the block had no statements, it has nil value"
	aRBBlockNode statements ifEmpty: [
		^ self pushOperand: nil asDRValue.
	].

	"Otherwise the block value is the value of its last expression"
	aRBBlockNode statements last acceptVisitor: self.
	self assert: self topFrame operandStack size = 1.
]

{ #category : 'API' }
DRIRGenerator >> interpretBlockClosure: aDRBlockClosure [

	"Create the initial basic block"

	| initialBasicBlock startBlock numArgs arguments |
	initialBasicBlock := self initialBasicBlock.
	startBlock := self newBasicBlock.
	startBlock jumpTo: initialBasicBlock.


	numArgs := aDRBlockClosure numberOfArguments.
	self numberOfArguments: numArgs.
	arguments := OrderedCollection new.
	numArgs > 0 ifTrue: [ 
		1 to: numArgs do: [ :i | 
			| loadInstruction |
			loadInstruction := startBlock loadArgument: i.
			loadInstruction argName: (aDRBlockClosure argName: i ).
			arguments add: loadInstruction ] ].

	self
		interpretCode: aDRBlockClosure
		receiver: aDRBlockClosure
		arguments: arguments.

	^ self ir
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBlockCullWith: aRBMessageNode [

	| block arguments |
	aRBMessageNode receiver acceptVisitor: self.
	block := self popOperand simplify.
	arguments := aRBMessageNode arguments collect: [ :e |
		e acceptVisitor: self.
		self popOperand ].
	
	"cull: only pass the required number of arguments and ignore the rest"
	self
		interpretCode: block
		receiver: block
		arguments: (arguments first: block numberOfArguments)
]

{ #category : 'special cases' }
DRIRGenerator >> interpretBlockValueWith: aRBMessageNode [

	| block arguments |
	aRBMessageNode receiver acceptVisitor: self.
	block := self popOperand simplify.
	block isDRBlockClosure ifFalse: [
		^ self resolveMessageSend: aRBMessageNode receiver: block ].

	arguments := aRBMessageNode arguments collect: [ :e |
		             e acceptVisitor: self.
		             self popOperand ].
	self interpretCode: block receiver: block arguments: arguments
]

{ #category : 'special cases' }
DRIRGenerator >> interpretCCodeInSmalltalkWith: aRBMessageNode [ 

	self interpretBlockBody: aRBMessageNode arguments first
]

{ #category : 'accessing' }
DRIRGenerator >> interpretCCoerceWith: aRBMessageNode [

	^ aRBMessageNode arguments first acceptVisitor: self
]

{ #category : 'special cases' }
DRIRGenerator >> interpretCaseOfWith: aRBMessageNode [ 
	
	| conditionValue previousFalseBlock trueBlocks otherwiseblock exitOtherwiseBlock continue |
	aRBMessageNode receiver acceptVisitor: self.
	conditionValue := self popOperand.
	
	previousFalseBlock := self currentBasicBlock.
	trueBlocks := aRBMessageNode arguments first statements collect: [ :case |
		| caseValue caseJump executionStateBeforeBranch executionStateAfterBranch |
		self assert: case selector = #->.
		
		"Case entry block"
		self newBasicBlock.
		previousFalseBlock jumpTo: currentBasicBlock.
		
		self interpretBlockBody: case receiver.
		caseValue := self popOperand.
		caseJump := self
			instantiateNoResultInstruction: DRBranchIfCondition
			operands: { DREqualsThanComparison new . conditionValue . caseValue }.
		self currentBasicBlock endInstruction: caseJump.
		
		"If condition does not hold block"
		previousFalseBlock := self newBasicBlock.
		currentBasicBlock addPredecessor: caseJump basicBlock.
		caseJump falseBranch: currentBasicBlock.
		
		"If condition holds block"
		executionStateBeforeBranch := executionState copy.
		self newBasicBlock.
		currentBasicBlock addPredecessor: caseJump basicBlock.
		caseJump trueBranch: currentBasicBlock.
		self interpretBlockBody: case arguments first.
		
		executionStateAfterBranch := executionState.
		executionState := executionStateBeforeBranch.
		{self currentBasicBlock. executionStateAfterBranch}
	].

	"otherwise block"
	otherwiseblock := self newBasicBlock.
	previousFalseBlock jumpTo: otherwiseblock.
	self interpretBlockBody: aRBMessageNode arguments second.
	exitOtherwiseBlock := self currentBasicBlock.
	
	continue := self newBasicBlock.
	exitOtherwiseBlock jumpTo: continue.
	trueBlocks do: [ :b | b first jumpTo: continue ].
	
	executionState := DRStackMerger new
		builder: self;
		mergeBlock: currentBasicBlock;
		mergeAll: { executionState }, (trueBlocks collect: [ :e | e second ]).
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretClassTableRootsObjectWith: aRBVariableNode [
	"TODO: interpretAsCompilerVariableWith:"

	^ self
		  interpretLoad: aRBVariableNode
		  ofAddress: DRInterpreterReference classTableFirstPage
		  ofType: (DRUnsignedIntegerType size: 8)
]

{ #category : 'API' }
DRIRGenerator >> interpretCode: aDRCode receiver: aReceiver arguments: arguments [

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

{ #category : 'special cases' }
DRIRGenerator >> interpretCondition: aRBMessageNode conditionKind: conditionKind [

	| operand1 operand2 |
	operand1 := self visitOperand: aRBMessageNode receiver.
	operand2 := self visitOperand: aRBMessageNode arguments first.

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRGetConditionCode
		  operands: { 
				  conditionKind new.
				  operand1.
				  operand2 }
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretCppIfWith: aRBMessageNode [

	| jitExpressionCopy |
	jitExpressionCopy := self visitOperand: aRBMessageNode arguments first.
	jitExpressionCopy simpleConstantFold
		ifTrue: [
			(RBMessageNode
				 receiver: aRBMessageNode arguments second
				 selector: #value) acceptVisitor: self ]
		ifFalse: [
			aRBMessageNode arguments size > 2 ifTrue: [
				(RBMessageNode
					 receiver: aRBMessageNode arguments third
					 selector: #value) acceptVisitor: self ] ]
]

{ #category : 'special cases' }
DRIRGenerator >> interpretDivisionWith: aRBMessageNode [ 

	"Interpret the two numbers and divide them up"
	| leftOperand rightOperand |

	leftOperand := self visitOperand: aRBMessageNode receiver.
	rightOperand := self visitOperand: aRBMessageNode arguments first.
	
	"Backend needs a third operand for rem result"
	self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRDivision 
		operands: { leftOperand . rightOperand }
]

{ #category : 'interpretation' }
DRIRGenerator >> interpretDruidExitPointWith: aRBMethodNode [

	self subclassResponsibility
]

{ #category : 'accessing' }
DRIRGenerator >> interpretDruidFailWith: aMessageNode [

	self error
]

{ #category : 'special cases' }
DRIRGenerator >> interpretEqualityComparisonWith: aRBMessageNode [

	^ self interpretCondition: aRBMessageNode conditionKind: DREqualsThanComparison
]

{ #category : 'special cases' }
DRIRGenerator >> interpretFalseObjectWith: aRBVariableNode [

	^ self addCopy: aRBVariableNode with: DRInterpreterReference falseObject
]

{ #category : 'special cases' }
DRIRGenerator >> interpretFloat64FromRawBitsWith: aRBMessageNode [

	| receiver |
	aRBMessageNode arguments first acceptVisitor: self.
	receiver := self popOperand.

	receiver type isIntegerType ifFalse: [
		^ self error: 'cannot extract float bits from a non float value' ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRGetFloatFromBits
		  operands: { receiver }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretFreeStartAssignmentWith: aRBAssignmentNode [

	| byteSize valueToStore |
	aRBAssignmentNode value acceptVisitor: self.
	valueToStore := self popOperand.

	byteSize := 8.
	self
		addInstructionWithNoResultFrom: aRBAssignmentNode
		instructionKind: DRStore
		operands: {
				byteSize asDRValue.
				valueToStore.
				DRInterpreterReference freeStartAddress }.

	"The result of executing a store is actually the stored value, not the store itself"
	self
		popOperand;
		pushOperand: valueToStore
]

{ #category : 'special cases' }
DRIRGenerator >> interpretFreeStartWith: aRBVariableNode [

	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRLoad
		  operands: {
				  DRPointerType new.
				  DRInterpreterReference freeStartAddress }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretGreaterOrEqualsThanComparisonWith: aRBMessageNode [ 
	
	^ self interpretCondition: aRBMessageNode conditionKind: DRGreaterOrEqualsThanComparison
]

{ #category : 'special cases' }
DRIRGenerator >> interpretGreaterThanComparisonWith: aRBMessageNode [ 
	
	^ self interpretCondition: aRBMessageNode conditionKind: DRGreaterThanComparison
]

{ #category : 'special cases' }
DRIRGenerator >> interpretHiddenRootsObjectWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference hiddenRootsObject
]

{ #category : 'special cases' }
DRIRGenerator >> interpretIfTrueWith: aRBMessageNode [

	aRBMessageNode receiver acceptVisitor: self.
	currentASTNode := aRBMessageNode.
	self
		buildIfTrue: self popOperand
		then: [ self interpretBlockBody: aRBMessageNode arguments first ]
]

{ #category : 'special cases' }
DRIRGenerator >> interpretInequalityComparisonWith: aRBMessageNode [
	
	^ self interpretCondition: aRBMessageNode conditionKind: DRNotEqualsThanComparison
]

{ #category : 'special cases' }
DRIRGenerator >> interpretLeftShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRLogicalLeftShift
]

{ #category : 'special cases' }
DRIRGenerator >> interpretLessOrEqualsThanComparisonWith: aRBMessageNode [ 

	^ self interpretCondition: aRBMessageNode conditionKind: DRLessOrEqualsThanComparison
]

{ #category : 'special cases' }
DRIRGenerator >> interpretLessThanComparisonWith: aRBMessageNode [ 

	^ self interpretCondition: aRBMessageNode conditionKind: DRLessThanComparison
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretLoad: aRBNode ofAddress: address ofType: aType [

	^ self
		  addInstructionFrom: aRBNode
		  instructionKind: DRLoad
		  operands: {
				  aType.
				  address }
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretLoad: aRBMessageNode ofType: aType [

	^ self
		  interpretLoad: aRBMessageNode
		  ofAddress: (self visitOperand: aRBMessageNode arguments first)
		  ofType: aType
]

{ #category : 'special cases' }
DRIRGenerator >> interpretLoadFloat64AtWith: aRBMessageNode [

	^ self interpretLoad: aRBMessageNode ofType: DRFloatType new
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretLoadUint16AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofType: (DRUnsignedIntegerType size: 2)
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretLoadUint32AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofType: (DRUnsignedIntegerType size: 4)
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretLoadUint64AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofType: (DRUnsignedIntegerType size: 8)
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretLoadUint8AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofType: (DRUnsignedIntegerType size: 1)
]

{ #category : 'special cases' }
DRIRGenerator >> interpretMachineWordAtPutWith: aRBMessageNode [

	self interpretStore: aRBMessageNode ofByteSize: 8  "Assume 64 bits for now"
]

{ #category : 'special cases' }
DRIRGenerator >> interpretMachineWordAtWith: aRBMessageNode [ 
	
	| address |
	
	address := self visitOperand: aRBMessageNode arguments first.
	^ self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRLoad
		operands: { 
			(DRUnsignedIntegerType size: 8 "word size...").
			address }.

]

{ #category : 'special cases' }
DRIRGenerator >> interpretModWith: aRBMessageNode [ 

	"Interpret the two numbers and push the module operation result"
	| leftOperand rightOperand |

	leftOperand := self visitOperand: aRBMessageNode receiver.
	rightOperand := self visitOperand: aRBMessageNode arguments first.
	
	"Backend needs a third operand for div result"
	self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRMod 
		operands: { leftOperand . rightOperand }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretMultiplySmallIntegerWithOverflowWith: aRBMessageNode [

	"Interpret the two numbers and multiply them up"

	| leftOperand rightOperand mulResult startingBasicBlock conditionalJump joinInstruction taggedResult |
	leftOperand := self visitOperand: aRBMessageNode arguments first.
	rightOperand := self visitOperand: aRBMessageNode arguments second.

	self unshiftSmallInteger: leftOperand from: aRBMessageNode.
	self untagSmallInteger: rightOperand from: aRBMessageNode.

	self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRMultiply
		operands: { 
				self popOperand.
				self popOperand }.

	mulResult := self popOperand.
	startingBasicBlock := self currentBasicBlock.
	conditionalJump := self
		                   instantiateNoResultInstruction:
		                   DRBranchIfCondition
		                   operands: { 
				                   DRMultiplyOverflow new.
				                   mulResult }.
	self currentBasicBlock endInstruction: conditionalJump.

	"No overflow"
	self newBasicBlock.
	self tagSmallInteger: mulResult from: aRBMessageNode.
	taggedResult := self popOperand.
	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump falseBranch: self currentBasicBlock.

	joinInstruction := self
		                   newOverflowBlockDuring: [ 
		                   self interpretBlockBody:
			                   aRBMessageNode arguments third ]
		                   fromJump: conditionalJump
		                   joinValue: taggedResult
]

{ #category : 'special cases' }
DRIRGenerator >> interpretMultiplyWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRMultiply
]

{ #category : 'special cases' }
DRIRGenerator >> interpretNegatedWith: aRBMessageNode [ 

	^ self interpretUnaryInstruction: aRBMessageNode instructionKind: DRNegate
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretNewSpaceMaskWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference getNewSpaceMask
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretNewSpaceStartWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference getNewSpaceStart
]

{ #category : 'special cases' }
DRIRGenerator >> interpretNilObjectWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference nilObject
]

{ #category : 'visiting' }
DRIRGenerator >> interpretNotWith: aRBMessageNode [ 
	self shouldBeImplemented.
]

{ #category : 'special cases' }
DRIRGenerator >> interpretObjectReferenceCoercionWith: aRBMessageNode [ 
	
	| resultToCAst |
	aRBMessageNode receiver acceptVisitor: self.
	
	"Add the type and repush"
	resultToCAst :=self popOperand.
	self assert: resultToCAst isLoad.
	resultToCAst type: DRObjectReferenceType new.
	self pushOperand: resultToCAst
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretOldSpaceMaskWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference getOldSpaceMask
]

{ #category : 'special cases' }
DRIRGenerator >> interpretOrWith: aRBMessageNode [ 
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DROr
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretPermSpaceMaskWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference getPermSpaceMask
]

{ #category : 'special cases' }
DRIRGenerator >> interpretPopThenPushWith: aRBMessageNode [ 

	"Pop N elements from the operand stack.
	Pushes the second argument to the stack.
	
	Pop: keep track of the operand stack and remove one element
	Push: move the value to the physical register assigned to the current stack position"
	
	| nElementsToPop elementToPush |
	aRBMessageNode arguments first acceptVisitor: self.
	nElementsToPop := self popOperand simpleConstantFold.
	nElementsToPop isInteger
		ifFalse: [ self error: 'expected an integer value' ].

	"If it's a pop with push we should evaluate the pushed argument before popping in case it uses the stack elements"
	aRBMessageNode arguments size > 1 ifTrue: [
		aRBMessageNode arguments second acceptVisitor: self.
		elementToPush := self popOperand ].
	
	self pop: nElementsToPop.
	aRBMessageNode arguments size > 1 ifTrue: [
		self push: elementToPush ].

	"pop:[thenPush:] does not return an interesting value"
	self pushOperand: DRNullValue new.
]

{ #category : 'special cases' }
DRIRGenerator >> interpretRawFloatBitsWith: aRBMessageNode [ 

	| receiver |
	aRBMessageNode arguments first acceptVisitor: self.
	receiver := self popOperand.

	receiver type isFloatType
		ifFalse: [ ^ self error: 'cannot extract float bits from a non float value' ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRGetFloatBits
		  operands: { receiver }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretRightShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRUnsignedRightShift
]

{ #category : 'special cases' }
DRIRGenerator >> interpretRotateLeftBy: aRBMessageNode [

	| operand1 operand2 |
	operand1 := self visitOperand: aRBMessageNode arguments first.
	operand2 := self visitOperand: aRBMessageNode arguments second.
	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRRotateLeft
		  operands: {
				  operand1.
				  operand2 }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretScavengeThresholdWith: aRBVariableNode [

	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRCopy
		  operands: { (DRInterpreterReference new reference:
				   'objectMemory getScavengeThreshold') }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretSizeofWith: aRBMessageNode [ 
	
	| receiver size |
	receiver := aRBMessageNode receiver acceptVisitor: self.
	size := self popOperand value sizeof: aRBMessageNode arguments first value.
	^ self addCopy: aRBMessageNode with: size asDRValue
]

{ #category : 'interpreting' }
DRIRGenerator >> interpretSpaceMaskToUseWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference getSpaceMaskToUse
]

{ #category : 'special cases - stack' }
DRIRGenerator >> interpretStackTopWith: aRBMessageNode [ 

	"self addInstruction: (self stackValue: 0) from: aRBMessageNode"
	self pushOperand: (self stackValue: 0)
]

{ #category : 'special cases - stack' }
DRIRGenerator >> interpretStackValueWith: aRBMessageNode [
	"Interpret messages of the form 
	interpreter stackValue: xxx.
	"

	| value |
	aRBMessageNode arguments first acceptVisitor: self.
	value := self popOperand simpleConstantFold.
	value isInteger ifFalse: [ self error: 'expected an integer value' ].

	self pushOperand: (self stackValue: value)
]

{ #category : 'special cases - stack' }
DRIRGenerator >> interpretStackValueWith: aRBMessageNode withValue: aValue [
	"Interpret messages of the form 
	interpreter stackValue: xxx.
	"

	aValue isInteger ifFalse: [ self error: 'expected an integer value' ].

	aValue = numberOfArguments ifTrue: [
		^ self
			  addInstructionFrom: aRBMessageNode
			  instructionKind: DRLoadReceiver
			  operands: #(  ) ].

	^ (self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRLoadArgument
		  operands: { (self argumentOffsetForStackValue: aValue) asDRValue })
			classType: (DRSignedIntegerType size: 8 "Word size")
]

{ #category : 'special cases' }
DRIRGenerator >> interpretStageableBlockWith: aRBMessageNode [

	| stageableBlock |
	aRBMessageNode arguments first acceptVisitor: self.
	stageableBlock := self popOperand simplify.
	stageableBlock isDRBlockClosure ifFalse: [ self error: 'Only consider blocks' ].

	stageableBlock blockNode arguments size > 0 ifTrue: [
		self error: 'Only consider blocks with no arguments' ].

	self stageInstructionsDuring: [
		self interpretCode: stageableBlock receiver: stageableBlock arguments: #(  ) ]
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretStore: aRBMessageNode ofByteSize: byteSize [

	| address value |
	address := self visitOperand: aRBMessageNode arguments first.
	value := self visitOperand: aRBMessageNode arguments second.
	self
		addInstructionWithNoResultFrom: aRBMessageNode
		instructionKind: DRStore
		operands: { 
				byteSize asDRValue.
				value.
				address }.

	"The result of executing a store is actually the stored value, not the store itself"
	self popOperand; pushOperand: value
]

{ #category : 'special cases' }
DRIRGenerator >> interpretStoreFloat64AtWith: aRBMessageNode [ 

	^ self interpretStore: aRBMessageNode ofByteSize: 8
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretStoreUint16AtWith: aRBMessageNode [ 
	
	^ self interpretStore: aRBMessageNode ofByteSize: 2
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretStoreUint32AtWith: aRBMessageNode [ 
	
	^ self interpretStore: aRBMessageNode ofByteSize: 4
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretStoreUint64AtWith: aRBMessageNode [ 
	
	^ self interpretStore: aRBMessageNode ofByteSize: 8
]

{ #category : 'special cases - memory access' }
DRIRGenerator >> interpretStoreUint8AtWith: aRBMessageNode [
	
	^ self interpretStore: aRBMessageNode ofByteSize: 1
]

{ #category : 'special cases' }
DRIRGenerator >> interpretSubSmallIntegerWithOverflowWith: aRBMessageNode [

	"Interpret the two numbers and subtract them up"

	| leftOperand rightOperand subResult startingBasicBlock conditionalJump |
	leftOperand := self visitOperand: aRBMessageNode arguments first.
	rightOperand := self visitOperand: aRBMessageNode arguments second.

	self untagSmallInteger: rightOperand from: aRBMessageNode.

	self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRSubtract
		operands: { 
				leftOperand.
				self popOperand }.
	subResult := self popOperand.
	startingBasicBlock := self currentBasicBlock.
	conditionalJump := self
		                   instantiateNoResultInstruction: DRBranchIfCondition
		                   operands: { DROverflow new . subResult }.
	self currentBasicBlock endInstruction: conditionalJump.

	self
		newOverflowBlockDuring: [ 
		self interpretBlockBody: aRBMessageNode arguments third ]
		fromJump: conditionalJump
		joinValue: subResult
]

{ #category : 'special cases' }
DRIRGenerator >> interpretSubstractWith: aRBMessageNode [ 
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRSubtract
]

{ #category : 'special cases' }
DRIRGenerator >> interpretSumSmallInteger: aRBMessageNode withOverflowCondition: condition [

	"Interpret the two numbers and add them up"

	| leftOperand rightOperand addResult startingBasicBlock conditionalJump joinInstruction |
	leftOperand := self visitOperand: aRBMessageNode arguments first.
	rightOperand := self visitOperand: aRBMessageNode arguments second.

	self untagSmallInteger: leftOperand from: aRBMessageNode.

	self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRAdd
		operands: { 
				self popOperand.
				rightOperand }.
	addResult := self popOperand.
	startingBasicBlock := self currentBasicBlock.
	conditionalJump := self
		                   instantiateNoResultInstruction:
		                   DRBranchIfCondition
		                   operands: { 
				                   condition.
				                   addResult }.
	self currentBasicBlock endInstruction: conditionalJump.

	joinInstruction := self
		                   newOverflowBlockDuring: [ 
			                   | block |
			                   aRBMessageNode arguments third acceptVisitor:
				                   self.
			                   block := self popOperand.
			                   self
				                   interpretCode: block
				                   receiver: block
				                   arguments: { addResult } ]
		                   fromJump: conditionalJump
		                   joinValue: addResult
]

{ #category : 'special cases' }
DRIRGenerator >> interpretSumSmallIntegerWithNoOverflowWith: aRBMessageNode [

	^ self
		  interpretSumSmallInteger: aRBMessageNode
		  withOverflowCondition: DRNoOverflow new
]

{ #category : 'special cases' }
DRIRGenerator >> interpretSumSmallIntegerWithOverflowWith: aRBMessageNode [

	^ self
		  interpretSumSmallInteger: aRBMessageNode
		  withOverflowCondition: DROverflow new
]

{ #category : 'special cases' }
DRIRGenerator >> interpretTrueObjectWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: DRInterpreterReference trueObject
]

{ #category : 'special cases' }
DRIRGenerator >> interpretUnaryInstruction: aRBMessageNode instructionKind: anInstructionKind [
	
	| receiver |
	receiver := self visitOperand: aRBMessageNode receiver.

	^ self addInstructionFrom: aRBMessageNode instructionKind: anInstructionKind operands: { receiver }
]

{ #category : 'special cases' }
DRIRGenerator >> interpretWhileTrueWith: aRBMessageNode [

	| trueBranchBasicBlockOut conditionalJump conditionEntryBlock executionStateAtEntry |
	"Compile
	
	[ condition ] whileTrue: [ something ]

	into
	
	condition-block:
		phis!
		condition
		jumpIfTrue body ifFalse exit
	
	body:
		something
		jump condition-block
	
	exit:
		...
	""Condition block(s)"
	self currentBasicBlock jumpTo:
		(conditionEntryBlock := self newBasicBlock).

	executionStateAtEntry := DRStackMerger new
		                         builder: self;
		                         mergeBlock: currentBasicBlock;
		                         mergeAll: { executionState }.

	executionState := executionStateAtEntry copy.
	self interpretBlockBody: aRBMessageNode receiver.

	"Could happen that the loop header block had a non local return.
	Otherwise, this block should back jump to the entry block of the loop"
	self currentBasicBlock hasFinalInstruction ifTrue: [
		self notYetImplemented ].

	"The conditional jump"
	conditionalJump := self
		                   instantiateNoResultInstruction:
		                   DRBranchIfCondition
		                   operands: {
				                   DREqualsThanComparison new.
				                   self popOperand.
				                   true asDRValue }.
	self currentBasicBlock endInstruction: conditionalJump.

	"Loop body. whileTrue: => jump if true to body"
	trueBranchBasicBlockOut := self
		                           branchFrom: self currentBasicBlock
		                           onEdge: [ :branchEntryBlock |
		                           conditionalJump trueBranch:
			                           branchEntryBlock ]
		                           doing: [
			                           aRBMessageNode arguments size > 0
				                           ifTrue: [
					                           self interpretBlockBody:
						                           aRBMessageNode arguments first.
					                           self popOperand ] ].

	"Could happen that any evaluated block had a non local return.
	Otherwise, this block should back jump to the entry block of the loop"
	trueBranchBasicBlockOut hasFinalInstruction ifFalse: [ "Now we should re-merge the outgoing phi values in the state of the entry block"
		executionState addTo: executionStateAtEntry.
		trueBranchBasicBlockOut backJumpTo: conditionEntryBlock ].

	"Else exit"
	self newBasicBlock.
	executionState := executionStateAtEntry.
	conditionalJump falseBranch: self currentBasicBlock.
	self currentBasicBlock addPredecessor: conditionalJump basicBlock.

	"whileTrue: does not return anything useful"
	self pushOperand: DRNullValue new
]

{ #category : 'accessing' }
DRIRGenerator >> ir [

	^ controlFlowGraph
]

{ #category : 'deferred returns' }
DRIRGenerator >> mergeDeferredReturns: deferredMethodReturns in: aDRBasicBlock [

	| frameReturns stackFrame |
	self flag: #TODO. "Is it ok to take the first one?"
	stackFrame := deferredMethodReturns values first topFrame.
	currentBasicBlock := aDRBasicBlock.
	frameReturns := deferredMethodReturns keys.
	frameReturns do: [ :frameReturn | 
		(aDRBasicBlock hasPredecessor: frameReturn basicBlock) ifTrue: [ "Remove to keep the order" 
			aDRBasicBlock removePredecessor: frameReturn basicBlock ].
		frameReturn basicBlock jumpTo: aDRBasicBlock ].
	self addInstructionFrom: stackFrame method instructionKind: DRPhiFunction operands: frameReturns.
	stackFrame returnValue: self popOperand.
	^ self addFrameReturn: stackFrame
]

{ #category : 'visiting' }
DRIRGenerator >> messageSendInstructionFor: aRBMessageNode receiver: receiver arguments: arguments method: method [

	| instruction |
	instruction := self
		               instantiate: DRMessageSend
		               operands: { receiver } , arguments.

	instruction inlineGenerator: self inlineGenerator.
	instruction methodNode: (method ifNotNil: [ method ast ]).
	^ self addInstruction: instruction from: aRBMessageNode
]

{ #category : 'visiting' }
DRIRGenerator >> methodFor: aRBMessageNode withReceiver: receiver [

	^ aRBMessageNode isSuperSend
		  ifTrue: [ 
			  self currentMethod methodNode methodClass superclass lookupSelector:
				  aRBMessageNode selector ]
		  ifFalse: [ receiver type lookupSelector: aRBMessageNode selector ]
]

{ #category : 'basic-blocks' }
DRIRGenerator >> newBasicBlock [

	| newBasicBlock |
	newBasicBlock := self basicNewBasicBlock.
	^ currentBasicBlock := newBasicBlock
]

{ #category : 'basic-blocks' }
DRIRGenerator >> newBasicBlockDuring: aFullBlockClosure [ 
	
	| inBasicBlock |
	inBasicBlock := self newBasicBlock.
	aFullBlockClosure value.
	^ inBasicBlock
]

{ #category : 'basic-blocks' }
DRIRGenerator >> newBasicBlockWithState: anExecutionState [

	| newBasicBlock |
	newBasicBlock := self basicNewBasicBlockWithState: anExecutionState.
	^ currentBasicBlock := newBasicBlock
]

{ #category : 'basic-blocks' }
DRIRGenerator >> newBasicBlockWithState: executionState during: aFullBlockClosure [ 
	
	| inBasicBlock |
	inBasicBlock := self newBasicBlockWithState: executionState.
	aFullBlockClosure value.
	^ inBasicBlock
]

{ #category : 'initialization' }
DRIRGenerator >> newCFG [
	
	^ DRControlFlowGraph new
]

{ #category : 'special cases' }
DRIRGenerator >> newOverflowBlockDuring: continuation fromJump: conditionalJump joinValue: result [

	| overflowBlock branchBlock resultBlock executionStateBeforeBranch overflowBlockOut |
	branchBlock := conditionalJump basicBlock.
	resultBlock := result basicBlock.

	self pushOperand: result.
	executionStateBeforeBranch := executionState copy.
	self popOperand.

	overflowBlock := self newBasicBlockDuring: continuation.
	overflowBlockOut := currentBasicBlock.
	conditionalJump trueBranch: overflowBlock.
	overflowBlock addPredecessor: branchBlock.

	"Merge point"
	self newBasicBlock.

	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	overflowBlockOut hasFinalInstruction
		ifTrue: [ executionState := executionStateBeforeBranch ]
		ifFalse: [ 
			executionState := DRStackMerger new
				                  builder: self;
				                  mergeBlock: currentBasicBlock;
				                  mergeAll: { 
						                  executionState.
						                  executionStateBeforeBranch }.
			overflowBlockOut jumpTo: self currentBasicBlock ].

	conditionalJump falseBranch
		ifNil: [ "If false branch doesnt exist then resultBlock = branchBlock"
			self currentBasicBlock addPredecessor: resultBlock.
			conditionalJump falseBranch: self currentBasicBlock ]
		ifNotNil: [ "If false branch exists then there is block in the middle" 
			resultBlock jumpTo: self currentBasicBlock ].

	^ self operandStackTop
]

{ #category : 'execution state' }
DRIRGenerator >> newVMState [
	
	^ DRVMState new
		initializeOnBuilder: self;
		yourself
]

{ #category : 'special cases' }
DRIRGenerator >> noop: aRBMessageNode [

	^ self
		  addInstructionWithNoResultFrom: aRBMessageNode
		  instructionKind: DRNoop
		  operands: #(  )
]

{ #category : 'accessing' }
DRIRGenerator >> numberOfArguments [

	^ numberOfArguments
]

{ #category : 'accessing' }
DRIRGenerator >> numberOfArguments: aValue [ 
	numberOfArguments := aValue.
]

{ #category : 'frame-access' }
DRIRGenerator >> operandStackTop [
	
	^ self topFrame stackTop
]

{ #category : 'pharo-stack' }
DRIRGenerator >> pop [

	self currentOperandStack ifEmpty: [ self error: 'Operand stack underflow!' ].
	^ self currentOperandStack pop
]

{ #category : 'pharo-stack' }
DRIRGenerator >> pop: anInteger [ 

	anInteger timesRepeat: [ self pop ]
]

{ #category : 'frame-access' }
DRIRGenerator >> popFrame [
	
	| poppedFrame |
	"Here we are in callee"

	poppedFrame := self popFrameMergingDeferredReturns.
	
	"Last frame!"
	executionState executionStack ifEmpty: [
		self finishCodeInterpretation: poppedFrame.
		^ poppedFrame ].
	
	"Here we are in caller"
	self pushOperand: poppedFrame returnValue.	
	
	^ poppedFrame
]

{ #category : 'frame-access' }
DRIRGenerator >> popFrameMergingDeferredReturns [

	^ self subclassResponsibility
]

{ #category : 'frame-access' }
DRIRGenerator >> popOperand [

	^ self topFrame pop
]

{ #category : 'pharo-stack' }
DRIRGenerator >> push: aValue [
	
	"Update the operand stack to keep track of the new position"
	^ self executionState pushVM: aValue
]

{ #category : 'stack management' }
DRIRGenerator >> pushFrame: aDruidStackFrame [ 
	
	executionState pushFrame: aDruidStackFrame
]

{ #category : 'stack management' }
DRIRGenerator >> pushFrameForCode: aDRCode receiver: aReceiver [

	^ aDRCode pushFrameIn: self receiver: aReceiver
]

{ #category : 'frame-access' }
DRIRGenerator >> pushOperand: anOperand [

	self topFrame push: anOperand
]

{ #category : 'frame-access' }
DRIRGenerator >> receiver [
	
	^ self topFrame receiver
]

{ #category : 'factory' }
DRIRGenerator >> receiverRegister [

	^ DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg'
]

{ #category : 'visiting' }
DRIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver [

	| arguments |
	arguments := aRBMessageNode arguments collect: [ :e | 
		             e acceptVisitor: self.
		             self popOperand ].

	self
		resolveMessageSend: aRBMessageNode
		receiver: receiver
		arguments: arguments
]

{ #category : 'visiting' }
DRIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	self subclassResponsibility 
]

{ #category : 'special cases' }
DRIRGenerator >> setExecutionStateTo: aBlock [

	self currentBasicBlock executionState: aBlock executionState.
	self topFrame executionState: aBlock executionState.
]

{ #category : 'pharo-stack' }
DRIRGenerator >> stackTop [
	
	^ self executionState topVM
]

{ #category : 'pharo-stack' }
DRIRGenerator >> stackValue: offset [
	
	^ self executionState valueVM: offset
]

{ #category : 'staging' }
DRIRGenerator >> stageInstructionsDuring: aFullBlockClosure [ 
	
	| oldStaging |
	oldStaging := staging.
	staging := true.
	aFullBlockClosure ensure: [ staging := oldStaging ]
]

{ #category : 'special cases' }
DRIRGenerator >> tagSmallInteger: operand from: aRBMessageNode [

	^ self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRAdd 
		operands: { operand . 1 asDRValue }.
	

]

{ #category : 'frame-access' }
DRIRGenerator >> temporaryAt: aString [ 
	
	^ self topFrame temporaryAt: aString withState: executionState
]

{ #category : 'frame-access' }
DRIRGenerator >> temporaryAt: aString put: aValue [ 

	^ self topFrame temporaryAt: aString put: aValue withState: executionState
]

{ #category : 'accessing' }
DRIRGenerator >> topFrame [

	^ executionState topFrame
]

{ #category : 'accessing' }
DRIRGenerator >> typeSystem [

	^ typeSystem
]

{ #category : 'accessing' }
DRIRGenerator >> typeSystem: anObject [

	typeSystem := anObject
]

{ #category : 'special cases' }
DRIRGenerator >> unshiftSmallInteger: operand from: aRBMessageNode [

	^ self 
		addInstructionFrom: aRBMessageNode
		instructionKind: DRSignedRightShift 
		operands: { operand . 3 asDRValue }.
]

{ #category : 'special cases' }
DRIRGenerator >> untagSmallInteger: operand from: aRBMessageNode [

	^ self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRSubtract
		operands: { operand . 1 asDRValue }.
	

]

{ #category : 'visiting' }
DRIRGenerator >> visitArgumentVariableNode: aRBVariableNode [ 
	
	^ self visitTemporaryVariableNode: aRBVariableNode 
]

{ #category : 'visiting' }
DRIRGenerator >> visitAssignmentNode: aRBAssignmentNode [

	variableSpecialCases at: aRBAssignmentNode variable name ifPresent: [ :mapping | 
		| rawMapping |
		rawMapping := mapping allButLast: 'With:' size.
		^ self perform: (rawMapping , 'AssignmentWith:') asSymbol withArguments: { aRBAssignmentNode } ].

	self interpretAssignmentNode: aRBAssignmentNode
	
]

{ #category : 'visiting' }
DRIRGenerator >> visitBlockNode: aRBBlockNode [

	self pushOperand: (DRBlockClosure new
			 blockNode: aRBBlockNode;
			 irGenerator: self blockClosureGenerator;
			 outerContext: self topFrame;
			 yourself)
]

{ #category : 'visiting' }
DRIRGenerator >> visitCascadeNode: aRBCascadeNode [ 
	

	| receiver result |
	aRBCascadeNode receiver acceptVisitor: self.
	receiver := self popOperand.
	
	aRBCascadeNode messages do: [ :e |
		self visitMessageNode: e receiver: receiver.
		result := self popOperand ].
	self pushOperand: result.
]

{ #category : 'visiting' }
DRIRGenerator >> visitClassVariableNode: aRBVariableNode [

	| value |
	value := aRBVariableNode variable value asDRValue.
	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRCopy
		operands: { value }
]

{ #category : 'as yet unclassified' }
DRIRGenerator >> visitInstanceVariableNode: aRBVariableNode [ 
	^ self subclassResponsibility
]

{ #category : 'visiting' }
DRIRGenerator >> visitLiteralValueNode: aRBLiteralValueNode [

	self
		addInstructionFrom: aRBLiteralValueNode
		instructionKind: DRCopy
		operands: { (DRConstantValue value: aRBLiteralValueNode value) }
]

{ #category : 'visiting' }
DRIRGenerator >> visitMessageNode: aRBMessageNode [

	| receiver |
	specialCases at: aRBMessageNode selector ifPresent: [ :mapping | 
		^ self perform: mapping withArguments: { aRBMessageNode } ].

	"Resolve it as a normal send"
	aRBMessageNode receiver acceptVisitor: self.
	receiver := self popOperand.
	self resolveMessageSend: aRBMessageNode receiver: receiver
]

{ #category : 'visiting' }
DRIRGenerator >> visitMessageNode: aRBMessageNode receiver: receiver [

	specialCases at: aRBMessageNode selector ifPresent: [ :mapping | 
		^ self perform: mapping withArguments: { aRBMessageNode } ].

	"Resolve it as a normal send"
	self resolveMessageSend: aRBMessageNode receiver: receiver
]

{ #category : 'visiting' }
DRIRGenerator >> visitMethodNode: aRBMethodNode [

	(aRBMethodNode hasPragmaNamed: #druidExitPoint) ifTrue: [
		^ self interpretDruidExitPointWith: aRBMethodNode ].


	(aRBMethodNode hasPragmaNamed: #doNotGenerate) ifTrue: [ 1 halt ].
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

{ #category : 'special cases' }
DRIRGenerator >> visitOperand: aDRValue [

	aDRValue acceptVisitor: self.
	^ self popOperand
]

{ #category : 'visiting' }
DRIRGenerator >> visitReturnNode: aRBReturnNode [

	| value homeFrame fork |

	value := self visitOperand: aRBReturnNode value.
	
	"Create a fork, push value operand and create a FrameReturn to keep DeferredReturnState ready to merge"
	fork := executionState copy.
	homeFrame := fork topFrame homeFrameWithState: fork.
	homeFrame push: value.
	homeFrame returnValue: value.
	self addDeferredReturnState: { 
			fork.
			self addFrameReturn: homeFrame }.
	
	executionState vmState lastStackInstructions: #().

	"The return is a statement with no value"
	self pushOperand: DRNullValue new
]

{ #category : 'visiting' }
DRIRGenerator >> visitSelfNode: aRBVariableNode [ 
	
	self pushOperand: (self topFrame receiverWithState: executionState)
]

{ #category : 'visiting' }
DRIRGenerator >> visitSuperNode: aRBVariableNode [ 
	
	self visitSelfNode: aRBVariableNode 
]

{ #category : 'visiting' }
DRIRGenerator >> visitTemporaryVariableNode: aRBVariableNode [ 
	
	^ self pushOperand: (self temporaryAt: aRBVariableNode name)
]
