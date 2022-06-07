Class {
	#name : #DRIRGenerator,
	#superclass : #Object,
	#instVars : [
		'specialCases',
		'interpretationStack',
		'currentBasicBlock',
		'firstBasicBlock',
		'numberOfArguments',
		'currentOperandStack',
		'currentPrimitiveFailureCode',
		'controlFlowGraph'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRIRGenerator >> addInstruction: instruction from: aNode [
	 
	instruction origin: thisContext sender stack.
	self currentBasicBlock addInstruction: instruction.
	instruction originAST: aNode.
	self pushOperand: instruction.
	^ instruction
]

{ #category : #visiting }
DRIRGenerator >> addInstructionFrom: aNode instructionKind: instructionClass operands: operands [
	
	| instruction |
	instruction := self
		instantiate: instructionClass
		operands: operands.
	^ self addInstruction: instruction from: aNode
]

{ #category : #visiting }
DRIRGenerator >> addInstructionWithNoResultFrom: aNode instructionKind: instructionClass operands: operands [
	
	| instruction |
	instruction := self
		instantiateNoResultInstruction: instructionClass
		operands: operands.
	^ self addInstruction: instruction from: aNode
]

{ #category : #factory }
DRIRGenerator >> allocateTemporaryRegister [
	
	^ self instructionFactory allocateTemporaryRegister
]

{ #category : #factory }
DRIRGenerator >> arg0register [
	
	^ self argRegisterNumber: 0
]

{ #category : #factory }
DRIRGenerator >> argRegisterNumber: anInteger [

	| argumentIndex |
	numberOfArguments = anInteger
		ifTrue: [ ^ self receiverRegister ].
	argumentIndex := numberOfArguments - anInteger - 1.
	^ DRPhysicalRegister name: 'Arg', argumentIndex asString, 'Reg'
]

{ #category : #'basic-blocks' }
DRIRGenerator >> basicNewBasicBlock [

	| block |
	block := controlFlowGraph newBasicBlock.
	
	block primitiveFailureCode: currentPrimitiveFailureCode.
	block vmOperandStack: currentOperandStack.
	block temporaryFrame: self currentTemporaryFrame.
	block origin: thisContext stack.
	
	^ block
]

{ #category : #'special-cases' }
DRIRGenerator >> branchFrom: startingBasicBlock onEdge: anEdgeBlock doing: aBlockToEvaluate [

	| branchEntryBlock branchExitBlock |
	
	"Evaluate a block using a copy of the operand stack and temporary frame.
	Copies are needed since branches make divergence points."
	currentOperandStack := startingBasicBlock vmOperandStack copy.
	currentPrimitiveFailureCode := self currentBasicBlock primitiveFailureCode.
	self currentTemporaryFrame: startingBasicBlock temporaryFrame copy.
	branchEntryBlock := self newBasicBlockDuring: aBlockToEvaluate.
	
	"Then update the successor and predecessor edges"
	anEdgeBlock value: branchEntryBlock.
	branchEntryBlock addPredecessor: startingBasicBlock.
	
	"Get the exit basic block, restore the starting basic block to simplify the caller's interaction"
	branchExitBlock := self currentBasicBlock.
	currentBasicBlock := startingBasicBlock.
	currentOperandStack := startingBasicBlock vmOperandStack.
	currentPrimitiveFailureCode := self currentBasicBlock primitiveFailureCode.
	self currentTemporaryFrame: startingBasicBlock temporaryFrame.
	
	^ branchExitBlock
]

{ #category : #'basic-blocks' }
DRIRGenerator >> currentBasicBlock [
	^ currentBasicBlock
]

{ #category : #'frame-access' }
DRIRGenerator >> currentFrame [
	
	^ interpretationStack last
]

{ #category : #lookup }
DRIRGenerator >> currentMethod [

	^ self currentFrame method
]

{ #category : #'frame-access' }
DRIRGenerator >> currentOperandStack [
	^ self currentBasicBlock vmOperandStack
]

{ #category : #'frame-access' }
DRIRGenerator >> currentTemporaryFrame [
	
	^ self currentFrame temporaryFrame
]

{ #category : #accessing }
DRIRGenerator >> currentTemporaryFrame: aCollection [

	^ self currentFrame temporaryFrame: aCollection
]

{ #category : #accessing }
DRIRGenerator >> initialBasicBlock [
	
	^ controlFlowGraph initialBasicBlock
]

{ #category : #initialization }
DRIRGenerator >> initialize [

	super initialize.
	controlFlowGraph := self newCFG.

	specialCases := Dictionary new.
	self initializeSpecialCases.
	
	currentOperandStack := Stack new.
	
	"Push a top frame"
	interpretationStack := OrderedCollection new.
	interpretationStack addLast: DruidTopFrame new
]

{ #category : #'special-cases' }
DRIRGenerator >> initializeSpecialCases [

	"Control flow and comparisons"
	specialCases at: #ifTrue:ifFalse: put: #interpretIfTrueifFalseWith:.
	specialCases at: #ifTrue: put: #interpretIfTrueWith:.
	specialCases at: #ifFalse: put: #interpretIfFalseWith:.
	specialCases at: #= put: #interpretEqualityComparisonWith:.
	specialCases at: #~= put: #interpretInequalityComparisonWith:.
	specialCases at: #< put: #interpretLessThanComparisonWith:.
	specialCases at: #<= put: #interpretLessOrEqualsThanComparisonWith:.
	specialCases at: #> put: #interpretGreaterThanComparisonWith:.
	specialCases at: #>= put: #interpretGreaterOrEqualsThanComparisonWith:.
	specialCases at: #or: put: #interpretOrWith:.
	specialCases at: #and: put: #interpretAndWith:.

	"Arithmetics for Numbers"
	specialCases at: #+ put: #interpretAddWith:.
	specialCases at: #- put: #interpretSubstractWith:.
	specialCases at: #* put: #interpretMultiplyWith:.
	specialCases at: #// put: #interpretDivisionWith:.
	specialCases at: #\\ put: #interpretModWith:.
	specialCases at: #negated put: #interpretNegatedWith:.
	
	"Arithmetics for SmallIntegers"
	specialCases at: #sumSmallInteger:withSmallInteger:ifOverflow: put: #interpretSumSmallIntegerWithOverflowWith:.
	specialCases at: #subSmallInteger:withSmallInteger:ifOverflow: put: #interpretSubSmallIntegerWithOverflowWith:.
	specialCases at: #multiplySmallInteger:withSmallInteger:ifOverflow: put: #interpretMultiplySmallIntegerWithOverflowWith:.


	"Type coercions"
	specialCases at: #asUnsignedInteger put: #interpretAsUnsignedIntegerWith:.

	"Memory accesses"
	specialCases at: #longAt: put: #interpretMachineWordAtWith:.
	self flag: #TODO. "Add types to Druid"
	specialCases at: #uint8At: put: #interpretLoadUint8AtWith:.
	specialCases at: #uint16At: put: #interpretLoadUint16AtWith:.
	specialCases at: #uint32At: put: #interpretLoadUint32AtWith:.
	specialCases at: #uint64At: put: #interpretLoadUint64AtWith:.
	specialCases at: #unsignedLongAt: put: #interpretMachineWordAtWith:.
	specialCases at: #unsignedLong64At: put: #interpretMachineWordAtWith:.
	specialCases at: #positive32BitIntegerFor: put: #interpretPrimFailCodeWith:. "integerObjectOf:"
	
	
	specialCases at: #longAt:put: put: #interpretMachineWordAtPutWith:.


	"Bit Manipulation"
	specialCases at: #<< put: #interpretLeftShiftWith:.
	specialCases at: #>> put: #interpretRightShiftWith:.
	specialCases at: #bitShift: put: #interpretBitShiftWith:.
	specialCases at: #bitAnd: put: #interpretBitAndWith:.
	specialCases at: #anyMask: put: #interpretAnyMaskWith:.

	"Stack access"
	specialCases at: #stackTop put: #interpretStackTopWith:.
	specialCases at: #stackValue: put: #interpretStackValueWith:.
	specialCases at: #pop:thenPush: put: #interpretPopThenPushWith:.
	
	"special variables"
	specialCases at: #nilObj put: #interpretNilObjectWith:.
	specialCases at: #falseObj put: #interpretFalseObjectWith:.
	specialCases at: #trueObj put: #interpretTrueObjectWith:.
	
	"Border case, this should not be defined here, it should be extensible"
	specialCases at: #hiddenRootsObj put: #interpretNilObjectWith:.
	
	"special simulation cases"
	specialCases at: #cCode:inSmalltalk: put: #interpretCCodeInSmalltalkWith:.
]

{ #category : #factory }
DRIRGenerator >> instantiate: anInstructionClass [ 
	
	^ self instantiate: anInstructionClass operands: #()
]

{ #category : #factory }
DRIRGenerator >> instantiate: anInstructionClass operands: aCollection [ 
	
	| instruction |
	instruction := anInstructionClass operands: aCollection result: self allocateTemporaryRegister.
	^ instruction
]

{ #category : #factory }
DRIRGenerator >> instantiateNoResultInstruction: anInstructionClass [
	
	^ self instantiateNoResultInstruction: anInstructionClass operands: #()
]

{ #category : #factory }
DRIRGenerator >> instantiateNoResultInstruction: anInstructionClass operands: aCollection [ 
	
	| instruction |
	instruction := anInstructionClass operands: aCollection.
	instruction origin: thisContext sender stack.
	^ instruction
]

{ #category : #accessing }
DRIRGenerator >> instructionFactory [
	^ controlFlowGraph instructionFactory
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretAddWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRAdd
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretAndWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRAnd
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretAnyMaskWith: aRBMessageNode [ 

	^ self interpretCondition: aRBMessageNode instructionKind: DRTest
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretAsUnsignedIntegerWith: aRBMessageNode [ 
	
	^ self interpretUnaryInstruction: aRBMessageNode instructionKind: DRUnsignedCoercion
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretBinaryInstruction: aRBMessageNode instructionKind: anInstructionKind [

	| operand1 operand2 |
	operand1 := self visitOperand: aRBMessageNode receiver.
	operand2 := self visitOperand: aRBMessageNode arguments first.

	^ self addInstructionFrom: aRBMessageNode instructionKind: anInstructionKind operands: { operand1 . operand2 }
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretBitAndWith: aRBMessageNode [
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRBitAnd
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretBitShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRBitShift
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretCCodeInSmalltalkWith: aRBMessageNode [ 
	
	^ aRBMessageNode arguments first acceptVisitor: self
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretCondition: aRBMessageNode instructionKind: anInstructionKind [

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: anInstructionKind
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretDivisionWith: aRBMessageNode [ 

	"Interpret the two numbers and divide them up"
	| leftOperand rightOperand divResult |

	leftOperand := self visitOperand: aRBMessageNode receiver.
	rightOperand := self visitOperand: aRBMessageNode arguments first.
	
	"Backend needs a third operand for rem result"
	divResult := self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRDivision 
		operands: { leftOperand . rightOperand }.

	self pushOperand: divResult 
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretEqualityComparisonWith: aRBMessageNode [

	^ self interpretCondition: aRBMessageNode instructionKind: DREqualsThanComparison
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretFalseObjectWith: aRBVariableNode [ 
	
	self pushOperand: DRFalseObject new
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretGreaterOrEqualsThanComparisonWith: aRBMessageNode [ 
	
	^ self interpretCondition: aRBMessageNode instructionKind: DRGreaterOrEqualsThanComparison
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretGreaterThanComparisonWith: aRBMessageNode [ 
	
	^ self interpretCondition: aRBMessageNode instructionKind: DRGreaterThanComparison
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfFalseWith: aRBMessageNode [

	| falseBranchBasicBlockOut startingBasicBlock conditionalJump |
	aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self
		instantiateNoResultInstruction: DRConditionalJump
		operands: { self popOperand }.
	
	self currentBasicBlock endInstruction: conditionalJump.
	
	falseBranchBasicBlockOut := self
		branchFrom: self currentBasicBlock
		onEdge: [ :branchEntryBlock | conditionalJump falseBranch: branchEntryBlock ]
		doing: [ aRBMessageNode arguments first acceptVisitor: self ].

	"Merge point"
	startingBasicBlock := self currentBasicBlock.
	self newBasicBlock.
	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump trueBranch: self currentBasicBlock.


	"ifTrue: does not return anything useful"
	self pushOperand: DRNullValue new.
	
	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	falseBranchBasicBlockOut hasFinalInstruction ifFalse: [
		falseBranchBasicBlockOut jumpTo: self currentBasicBlock.
	].

	currentOperandStack := self currentBasicBlock mergeOperandStacksIn: self.
	currentPrimitiveFailureCode := self currentBasicBlock primitiveFailureCode.
	self currentTemporaryFrame: (self currentBasicBlock mergeTemporaryFramesIn: self).
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfTrueWith: aRBMessageNode [

	| trueBranchBasicBlockOut conditionalJump startingBasicBlock |
	aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self
		instantiateNoResultInstruction: DRConditionalJump
		operands: { self popOperand }.
	
	self currentBasicBlock endInstruction: conditionalJump.

	trueBranchBasicBlockOut := self
		branchFrom: self currentBasicBlock
		onEdge: [ :branchEntryBlock | conditionalJump trueBranch: branchEntryBlock ]
		doing: [ aRBMessageNode arguments first acceptVisitor: self ].

	"Merge point"
	startingBasicBlock := self currentBasicBlock.
	self newBasicBlock.
	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump falseBranch: self currentBasicBlock.
		
	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	trueBranchBasicBlockOut hasFinalInstruction ifFalse: [
		trueBranchBasicBlockOut jumpTo: self currentBasicBlock.
	].

	currentOperandStack := self currentBasicBlock mergeOperandStacksIn: self.
	currentPrimitiveFailureCode := self currentBasicBlock primitiveFailureCode.
	self currentTemporaryFrame: (self currentBasicBlock mergeTemporaryFramesIn: self).
	
	"ifTrue: does not return anything useful"
	self pushOperand: DRNullValue new.

]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfTrueifFalseWith: aRBMessageNode [

	| trueBranchBasicBlockOut falseBranchBasicBlockOut joinInstruction conditionalJump  |
	aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self
		                   instantiateNoResultInstruction: DRConditionalJump
		                   operands: { self popOperand }.
	self currentBasicBlock endInstruction: conditionalJump.

	trueBranchBasicBlockOut := self
		branchFrom: self currentBasicBlock
		onEdge: [ :branchEntryBlock | conditionalJump trueBranch: branchEntryBlock ]
		doing: [ aRBMessageNode arguments first acceptVisitor: self ].

	falseBranchBasicBlockOut := self
		branchFrom: self currentBasicBlock
		onEdge: [ :branchEntryBlock | conditionalJump falseBranch: branchEntryBlock ]
		doing: [ aRBMessageNode arguments second acceptVisitor: self ].
	
	"Start a new operand stack at this point"
	self newBasicBlock.
	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"

	trueBranchBasicBlockOut hasFinalInstruction ifFalse: [
		trueBranchBasicBlockOut jumpTo: self currentBasicBlock.
	].
	falseBranchBasicBlockOut hasFinalInstruction ifFalse: [
		falseBranchBasicBlockOut jumpTo: self currentBasicBlock.
	].

	"join the values returned by the blocks"
	joinInstruction := trueBranchBasicBlockOut blockValue
		                   joinValue: falseBranchBasicBlockOut blockValue
		                   onIRGenerator: self
		                   from: { 
				                   trueBranchBasicBlockOut.
				                   falseBranchBasicBlockOut }.

	currentOperandStack := self currentBasicBlock mergeOperandStacksIn: self.
	currentPrimitiveFailureCode := self currentBasicBlock primitiveFailureCode.
	self currentTemporaryFrame: (self currentBasicBlock mergeTemporaryFramesIn: self).
	self pushOperand: joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretInequalityComparisonWith: aRBMessageNode [
	
	^ self interpretCondition: aRBMessageNode instructionKind: DRNotEqualsThanComparison
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretLeftShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRLeftShift
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretLessOrEqualsThanComparisonWith: aRBMessageNode [ 

	^ self interpretCondition: aRBMessageNode instructionKind: DRLessOrEqualsThanComparison
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretLessThanComparisonWith: aRBMessageNode [ 

	^ self interpretCondition: aRBMessageNode instructionKind: DRLessThanComparison
]

{ #category : #'special cases - memory access' }
DRIRGenerator >> interpretLoad: aRBMessageNode ofByteSize: byteSize [

	| result address |
	address := self visitOperand: aRBMessageNode arguments first.

	result := self
		          addInstructionFrom: aRBMessageNode
		          instructionKind: DRLoad
		          operands: { 
				          byteSize asDRValue.
				          address }.

	self pushOperand: result
]

{ #category : #'special cases - memory access' }
DRIRGenerator >> interpretLoadUint16AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofByteSize: 2
]

{ #category : #'special cases - memory access' }
DRIRGenerator >> interpretLoadUint32AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofByteSize: 4
]

{ #category : #'special cases - memory access' }
DRIRGenerator >> interpretLoadUint64AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofByteSize: 8
]

{ #category : #'special cases - memory access' }
DRIRGenerator >> interpretLoadUint8AtWith: aRBMessageNode [ 
	
	^ self interpretLoad: aRBMessageNode ofByteSize: 1
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretMachineWordAtPutWith: aRBMessageNode [ 
	
	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	
	^ self currentBasicBlock addInstruction: (DRStore operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretMachineWordAtWith: aRBMessageNode [ 
	
	| result address |
	
	address := self visitOperand: aRBMessageNode arguments first.
	
	result := self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRLoad
		operands: { 
			8 asDRValue. "Assume 64 bits for now"
			address }.

	self pushOperand: result
]

{ #category : #API }
DRIRGenerator >> interpretMethod: aMethodAST receiver: aReceiver [

	^ self interpretMethod: aMethodAST receiver: aReceiver arguments: #()
]

{ #category : #API }
DRIRGenerator >> interpretMethod: aMethodAST receiver: aReceiver arguments: arguments [

	self pushFrameWithReceiver: aReceiver method: aMethodAST.
	aMethodAST argumentNames with: arguments do: [ :name :value |
		self temporaryAt: name put: value
	].
	aMethodAST acceptVisitor: self.

	"Fall through the exit block.
	If the method had an explicit return, it is already done, no need to do it again."
	currentBasicBlock hasFinalInstruction ifFalse: [ 
		currentBasicBlock jumpTo: self currentFrame exitBasicBlock ].

	self popFrame.
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretModWith: aRBMessageNode [ 

	"Interpret the two numbers and push the module operation result"
	| leftOperand rightOperand modResult |

	leftOperand := self visitOperand: aRBMessageNode receiver.
	rightOperand := self visitOperand: aRBMessageNode arguments first.
	
	"Backend needs a third operand for div result"
	modResult := self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRMod 
		operands: { leftOperand . rightOperand }.

	self pushOperand: modResult
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretMultiplySmallIntegerWithOverflowWith: aRBMessageNode [

	"Interpret the two numbers and multiply them up"
	| leftOperand rightOperand mulResult startingBasicBlock conditionalJump joinInstruction unshiftedLeftOperand withoutTagRightOperand taggedResult |
	
	leftOperand := self visitOperand: aRBMessageNode arguments first.
	rightOperand := self visitOperand: aRBMessageNode arguments second.
	
	unshiftedLeftOperand := self unshiftSmallInteger: leftOperand from: aRBMessageNode.
	withoutTagRightOperand := self untagSmallInteger: rightOperand from: aRBMessageNode.
	
	mulResult := self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRMultiply 
		operands: { unshiftedLeftOperand . withoutTagRightOperand }.

	startingBasicBlock := self currentBasicBlock.	
	conditionalJump := self
		instantiateNoResultInstruction: DRMultiplyOverflowJump
		operands: { mulResult }.
	self currentBasicBlock endInstruction: conditionalJump.

	"No overflow"
	self newBasicBlock.
	taggedResult := self tagSmallInteger: mulResult from: aRBMessageNode.
	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump falseBranch: self currentBasicBlock.

	joinInstruction := self 
		newOverflowBlockDuring:  [ 
			 aRBMessageNode arguments third acceptVisitor: self 
			]
		fromJump: conditionalJump
		joinValue: taggedResult.
	
	self pushOperand: joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretMultiplyWith: aRBMessageNode [ 

	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (DRMultiply operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretNegatedWith: aRBMessageNode [ 

	^ self interpretUnaryInstruction: aRBMessageNode instructionKind: DRNegate
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretNilObjectWith: aRBVariableNode [ 
	
	self pushOperand: DRNilObject new
]

{ #category : #visiting }
DRIRGenerator >> interpretNotWith: aRBMessageNode [ 
	self shouldBeImplemented.
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretOrWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DROr
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretPopThenPushWith: aRBMessageNode [ 

	"Pop N elements from the operand stack.
	Pushes the second argument to the stack.
	
	Pop: keep track of the operand stack and remove one element
	Push: move the value to the physical register assigned to the current stack position"
	
	| elementsToPop elementsToPopNode |
	aRBMessageNode arguments first acceptVisitor: self.
	elementsToPopNode := self popOperand.
	elementsToPop := elementsToPopNode simpleConstantFold.
	elementsToPop isInteger
		ifFalse: [ self error: 'expected an integer value' ].
	"This will be pushed automatically"
	aRBMessageNode arguments second acceptVisitor: self.
	self pop: elementsToPop.
	self push: self popOperand.

	"pop:thenPush: does not return an interesting value"
	self pushOperand: DRNullValue new.
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretRightShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRRightShift
]

{ #category : #'special cases - stack' }
DRIRGenerator >> interpretStackTopWith: aRBMessageNode [ 

	self pushOperand: self currentOperandStack top
]

{ #category : #'special cases - stack' }
DRIRGenerator >> interpretStackValueWith: aRBMessageNode [

	"Interpret messages of the form 
	interpreter stackValue: xxx.
	"	
	| value |
	aRBMessageNode arguments first acceptVisitor: self.
	value := self popOperand.
	value operand1 value isInteger ifFalse: [ self error: 'expected an integer value' ].
	
	^ self addInstructionFrom: aRBMessageNode instructionKind: DRCopy operands: { self argRegisterNumber: value operand1 value }
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretSubSmallIntegerWithOverflowWith: aRBMessageNode [

	"Interpret the two numbers and subtract them up"
	| leftOperand rightOperand subResult startingBasicBlock conditionalJump joinInstruction untaggedRightOperand |
	
	leftOperand := self visitOperand: aRBMessageNode arguments first.
	rightOperand := self visitOperand: aRBMessageNode arguments second.
	
	untaggedRightOperand := self untagSmallInteger: rightOperand from: aRBMessageNode.
	
	subResult := self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRSubtract
		operands: { leftOperand . untaggedRightOperand }.

	startingBasicBlock := self currentBasicBlock.	
	conditionalJump := self instantiateNoResultInstruction: DROverflowJump operands: { subResult }.
	self currentBasicBlock endInstruction: conditionalJump.
	
	joinInstruction := self 
		newOverflowBlockDuring:  [ 
			 aRBMessageNode arguments third acceptVisitor: self 
			]
		fromJump: conditionalJump
		joinValue: subResult.
	
	self pushOperand: joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretSubstractWith: aRBMessageNode [ 
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRSubtract
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretSumSmallIntegerWithOverflowWith: aRBMessageNode [

	"Interpret the two numbers and add them up"
	| leftOperand rightOperand addResult startingBasicBlock conditionalJump joinInstruction untaggedLeftOperand |
	
	leftOperand := self visitOperand: aRBMessageNode arguments first.
	rightOperand := self visitOperand: aRBMessageNode arguments second.
	
	untaggedLeftOperand := self untagSmallInteger: leftOperand from: aRBMessageNode.
	
	addResult := self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRAdd
		operands: { untaggedLeftOperand . rightOperand }.

	startingBasicBlock := self currentBasicBlock.	
	conditionalJump := self instantiateNoResultInstruction: DROverflowJump operands: { addResult }.
	self currentBasicBlock endInstruction: conditionalJump.

	joinInstruction := self 
		newOverflowBlockDuring:  [ 
			 aRBMessageNode arguments third acceptVisitor: self 
			]
		fromJump: conditionalJump
		joinValue: addResult.
	
	self pushOperand: joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretTrueObjectWith: aRBVariableNode [ 

	^ self addInstructionFrom: aRBVariableNode instructionKind: DRCopy operands: { DRTrueObject new }
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretUnaryInstruction: aRBMessageNode instructionKind: anInstructionKind [
	
	| receiver |
	receiver := self visitOperand: aRBMessageNode receiver.

	^ self addInstructionFrom: aRBMessageNode instructionKind: anInstructionKind operands: { receiver }
]

{ #category : #accessing }
DRIRGenerator >> ir [
	^ self subclassResponsibility
]

{ #category : #'basic-blocks' }
DRIRGenerator >> newBasicBlock [

	| newBasicBlock |
	newBasicBlock := self basicNewBasicBlock.
	^ currentBasicBlock := newBasicBlock
]

{ #category : #'basic-blocks' }
DRIRGenerator >> newBasicBlockDuring: aFullBlockClosure [ 
	
	| inBasicBlock |
	inBasicBlock := self newBasicBlock.
	aFullBlockClosure value.
	^ inBasicBlock
]

{ #category : #initialization }
DRIRGenerator >> newCFG [
	
	^ DRControlFlowGraph new
]

{ #category : #'special-cases' }
DRIRGenerator >> newOverflowBlockDuring: continuation fromJump: conditionalJump joinValue: result [

	| overflowBlock branchBlock resultBlock joinInstruction |
	branchBlock := conditionalJump basicBlock. 
	resultBlock := result basicBlock.

	overflowBlock := self newBasicBlockDuring: continuation.
	conditionalJump trueBranch: overflowBlock.
	overflowBlock addPredecessor: branchBlock.

	"Merge point"
	self newBasicBlock.
	
	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	overflowBlock hasFinalInstruction ifFalse: [
		overflowBlock jumpTo: self currentBasicBlock.
	].

	conditionalJump falseBranch 
		ifNil: [ 
			"If false branch doesnt exist then resultBlock = branchBlock"
			self currentBasicBlock addPredecessor: resultBlock.
			conditionalJump falseBranch: self currentBasicBlock.
		] 
		ifNotNil: [
			"If false branch exists then there is block in the middle"
			resultBlock jumpTo: self currentBasicBlock.
		].

	joinInstruction := overflowBlock blockValue
		joinValue: result
		onIRGenerator: self
		from: { overflowBlock . resultBlock }.

	currentOperandStack := self currentBasicBlock mergeOperandStacksIn: self.
	currentPrimitiveFailureCode := self currentBasicBlock primitiveFailureCode.
	self currentTemporaryFrame: (self currentBasicBlock mergeTemporaryFramesIn: self).
	
	^ joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> noop: aRBMessageNode [

	^ self
		  addInstructionWithNoResultFrom: aRBMessageNode
		  instructionKind: DRNoop
		  operands: #(  )
]

{ #category : #'frame-access' }
DRIRGenerator >> operandStackTop [
	
	^ self currentFrame stackTop
]

{ #category : #'pharo-stack' }
DRIRGenerator >> pop [

	self currentOperandStack ifEmpty: [ self error: 'Operand stack underflow!' ].
	^ self currentOperandStack pop
]

{ #category : #'pharo-stack' }
DRIRGenerator >> pop: anInteger [ 

	anInteger timesRepeat: [ self pop ]
]

{ #category : #'frame-access' }
DRIRGenerator >> popFrame [
	
	| poppedFrame "returnValue" |
	currentBasicBlock := self currentFrame exitBasicBlock.
	currentOperandStack := currentBasicBlock mergeOperandStacksIn: self.
	currentPrimitiveFailureCode := currentBasicBlock primitiveFailureCode.
	poppedFrame := interpretationStack removeLast.

	currentBasicBlock addInstruction: poppedFrame returnValue.
	"returnValue := poppedFrame returnValue simplify."
	self pushOperand: poppedFrame returnValue.
	
	self currentTemporaryFrame: currentBasicBlock temporaryFrame.
	currentBasicBlock jumpTo: self newBasicBlock.
]

{ #category : #'frame-access' }
DRIRGenerator >> popOperand [

	^ self currentFrame pop
]

{ #category : #'pharo-stack' }
DRIRGenerator >> push: aValue [
	
	"Update the operand stack to keep track of the new position"
	self currentOperandStack push: aValue.
	"Each position of the operand stack is related to a special register.
	Translate the push to a copy with a concrete result register.
	The concrete result register will be the one associated with the current position of the operand stack"
	"self haltIf: [ aValue isKindOf: DRNullValue ].
	copyInstruction := self instantiate: DRCopy operands: { aValue }.
	destinationRegister := operandStackPosition = 1
		ifTrue: [ self receiverRegister ]
		ifFalse: [ DRPhysicalRegister name: 'Arg', (operandStackPosition - 2) asString, 'Reg' ].
	
	copyInstruction result: destinationRegister.
	^ self currentBasicBlock addInstruction: copyInstruction"
]

{ #category : #'frame-access' }
DRIRGenerator >> pushFrameWithReceiver: aReceiver method: aMethod [
	
	| frame newMethodEntry |
	newMethodEntry := self basicNewBasicBlock.
	self currentBasicBlock jumpTo: newMethodEntry.
	currentBasicBlock := newMethodEntry.
	
	frame := DruidStackFrame new
		receiver: aReceiver;
		method: aMethod;
		irGenerator: self;
		entryBasicBlock: newMethodEntry;
		exitBasicBlock: self basicNewBasicBlock;
		yourself.
	newMethodEntry temporaryFrame: frame temporaryFrame.
	
	interpretationStack addLast: frame.
	currentBasicBlock
		temporaryAt: '__return'
		put: (DRPhiFunction new
			result: self allocateTemporaryRegister;
			origin: thisContext stack;
			yourself).
]

{ #category : #'frame-access' }
DRIRGenerator >> pushOperand: anOperand [

	self currentFrame push: anOperand
]

{ #category : #'frame-access' }
DRIRGenerator >> receiver [
	
	^ self currentFrame receiver
]

{ #category : #factory }
DRIRGenerator >> receiverRegister [
	
	^ DRPhysicalRegister name: 'ReceiverResultReg'
]

{ #category : #'special-cases' }
DRIRGenerator >> shiftSmallInteger: operand from: aRBMessageNode [

	^ self 
		addInstructionFrom: aRBMessageNode
		instructionKind: DRLeftShift  
		operands: { 3 asDRValue . operand }.
]

{ #category : #'pharo-stack' }
DRIRGenerator >> spillStack [

	"If there is no operand stack, the current block has no predecessors and is considered as dead code.
	Do not spill the stack here."
	self currentOperandStack ifNil: [ ^ self ].
	self currentOperandStack size > (numberOfArguments + 1)
		ifTrue: [ self error: 'Non Balanced Stack!' ].

	[ self currentOperandStack isEmpty ] whileFalse: [ | destinationRegister copyInstruction |
		destinationRegister := self currentOperandStack size = 1
			ifTrue: [ self receiverRegister ]
			ifFalse: [ DRPhysicalRegister name: 'Arg', (self currentOperandStack size - 2) asString, 'Reg' ].
		copyInstruction := DRCopy
			operands: { self currentOperandStack pop }
			result: destinationRegister.
		self currentBasicBlock addInstruction: copyInstruction.
	]
]

{ #category : #'pharo-stack' }
DRIRGenerator >> stackTop [
	
	^ self currentOperandStack top
]

{ #category : #'special-cases' }
DRIRGenerator >> tagSmallInteger: operand from: aRBMessageNode [

	^ self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRAdd 
		operands: { operand . 1 asDRValue }.
	

]

{ #category : #'frame-access' }
DRIRGenerator >> temporaryAt: aString [ 
	
	^ self currentBasicBlock temporaryAt: aString
]

{ #category : #'frame-access' }
DRIRGenerator >> temporaryAt: aString put: aValue [ 

	^ self currentBasicBlock temporaryAt: aString put: aValue
]

{ #category : #'special-cases' }
DRIRGenerator >> unshiftSmallInteger: operand from: aRBMessageNode [

	^ self 
		addInstructionFrom: aRBMessageNode
		instructionKind: DRRightShift 
		operands: { operand . 3 asDRValue }.
]

{ #category : #'special-cases' }
DRIRGenerator >> untagSmallInteger: operand from: aRBMessageNode [

	^ self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRSubtract
		operands: { operand . 1 asDRValue }.
	

]

{ #category : #visiting }
DRIRGenerator >> visitArgumentVariableNode: aRBVariableNode [ 
	
	^ self visitTemporaryVariableNode: aRBVariableNode 
]

{ #category : #visiting }
DRIRGenerator >> visitAssignmentNode: aRBAssignmentNode [ 
	
	specialCases
		at: aRBAssignmentNode variable name
		ifPresent: [ :mapping | | rawMapping |
			rawMapping := mapping allButLast: 'With:' size.
			^ self perform: (rawMapping , 'AssignmentWith:') asSymbol withArguments: { aRBAssignmentNode } ].
	
	aRBAssignmentNode value acceptVisitor: self.
	
	aRBAssignmentNode variable binding isTempVariable ifTrue: [ 
		^ self
			temporaryAt: aRBAssignmentNode variable name
			put: self operandStackTop ].
	
	aRBAssignmentNode variable binding isInstanceVariable ifTrue: [ 
		^ self receiver value
			instVarNamed: aRBAssignmentNode variable name
			put: self operandStackTop ].
	
	self halt
]

{ #category : #visiting }
DRIRGenerator >> visitBlockNode: aRBBlockNode [
	
	| blockValue |

	aRBBlockNode statements do: [ :e |
		e acceptVisitor: self.
		e isReturn ifTrue: [ ^ self ].
		blockValue := self popOperand.
	].
	"At the end, transform the block value into a copy instruction"
	blockValue := self
		addInstructionFrom: aRBBlockNode
		instructionKind: DRCopy
		operands: {blockValue}.
	
	"The add instruction pushes the instruction to the operand stack.
	Pop it: at the end of the block execution, nothing should be in the stack"
	self currentBasicBlock blockValue: blockValue
]

{ #category : #visiting }
DRIRGenerator >> visitClassVariableNode: aRBVariableNode [ 

	self pushOperand: aRBVariableNode variable value asDRValue
]

{ #category : #visiting }
DRIRGenerator >> visitInstanceVariableNode: aRBVariableNode [ 

	specialCases
		at: aRBVariableNode name
		ifPresent: [ :mapping |
			^ self perform: mapping withArguments: { aRBVariableNode } ].
	
	"Resolve it as a variableAccess"
	self pushOperand: (self receiver value instVarNamed: aRBVariableNode name) asDRValue
]

{ #category : #visiting }
DRIRGenerator >> visitLiteralValueNode: aRBLiteralValueNode [

	self
		addInstructionFrom: aRBLiteralValueNode
		instructionKind: DRCopy
		operands: { (DRConstantValue value: aRBLiteralValueNode value) }
]

{ #category : #visiting }
DRIRGenerator >> visitMessageNode: aRBMessageNode [ 

	| receiver arguments method |
	specialCases
		at: aRBMessageNode selector
		ifPresent: [ :mapping |
			^ self perform: mapping withArguments: { aRBMessageNode } ].
	
	"Resolve it as a normal send"
	aRBMessageNode receiver acceptVisitor: self.
	receiver := self popOperand.
	arguments := aRBMessageNode arguments collect: [ :e |
		e acceptVisitor: self.
		self popOperand ].
	method := aRBMessageNode isSuperSend 
					ifTrue: [ self currentMethod methodClass superclass lookupSelector: aRBMessageNode selector ]
					ifFalse: [ receiver lookupClass lookupSelector: aRBMessageNode selector ].
	self interpretMethod: method ast receiver: receiver arguments: arguments
]

{ #category : #visiting }
DRIRGenerator >> visitMethodNode: aRBMethodNode [ 
	
	aRBMethodNode statements do: [ :each |
		each acceptVisitor: self.
		each isReturn ifTrue: [ ^ self ].
		self popOperand. ].

	"If the method has no return statement, it should still return something"
	self currentFrame returnValue: DRNullValue new from: self currentBasicBlock
]

{ #category : #'special-cases' }
DRIRGenerator >> visitOperand: aDRValue [

	aDRValue acceptVisitor: self.
	^ self popOperand
]

{ #category : #visiting }
DRIRGenerator >> visitReturnNode: aRBReturnNode [

	| result |
	aRBReturnNode value acceptVisitor: self.
	result := self popOperand copyToVariableIn: self inNode: aRBReturnNode.
	self currentFrame returnValue: result from: self currentBasicBlock.
	self currentBasicBlock jumpTo: self currentFrame exitBasicBlock
]

{ #category : #visiting }
DRIRGenerator >> visitSelfNode: aRBVariableNode [ 
	
	self pushOperand: self receiver
]

{ #category : #visiting }
DRIRGenerator >> visitSuperNode: aRBVariableNode [ 
	
	self visitSelfNode: aRBVariableNode 
]

{ #category : #visiting }
DRIRGenerator >> visitTemporaryVariableNode: aRBVariableNode [ 
	
	^ self pushOperand: (self temporaryAt: aRBVariableNode name)
]
