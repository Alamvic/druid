Class {
	#name : #DRIRGenerator,
	#superclass : #Object,
	#instVars : [
		'specialCases',
		'interpretationStack',
		'currentBasicBlock',
		'firstBasicBlock',
		'numberOfArguments',
		'nextRegister',
		'nextBlockID',
		'pharoOperandStack',
		'currentOperandStack'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRIRGenerator >> addInstructionFrom: aNode instructionKind: instructionClass operands: operands [
	
	| instruction |
	self haltIf: [ operands anySatisfy: [ :op | (op isKindOf: DRValue) not ] ].
	instruction := self
		instantiate: instructionClass
		operands: operands.
	self currentBasicBlock addInstruction: instruction.
	instruction originAST: aNode.
	self pushOperand: instruction.
	^ instruction
]

{ #category : #visiting }
DRIRGenerator >> addInstructionWithNoResultFrom: aNode instructionKind: instructionClass operands: operands [
	
	| instruction |
	instruction := self
		instantiateNoResultInstruction: instructionClass
		operands: operands.
	self currentBasicBlock addInstruction: instruction.
	instruction originAST: aNode.
	self pushOperand: instruction.
	^ instruction
]

{ #category : #factory }
DRIRGenerator >> allocateTemporaryRegister [
	
	| allocatedRegisterId |
	allocatedRegisterId := nextRegister.
	nextRegister := nextRegister + 1.
	^ DRTemporaryRegister id: allocatedRegisterId
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
	block := DRBasicBlock id: self nextBlockID.
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
	self currentTemporaryFrame: startingBasicBlock temporaryFrame copy.
	branchEntryBlock := self newBasicBlockDuring: aBlockToEvaluate.
	
	"Then update the successor and predecessor edges"
	anEdgeBlock value: branchEntryBlock.
	branchEntryBlock addPredecessor: startingBasicBlock.
	
	"Get the exit basic block, restore the starting basic block to simplify the caller's interaction"
	branchExitBlock := self currentBasicBlock.
	currentBasicBlock := startingBasicBlock.
	currentOperandStack := startingBasicBlock vmOperandStack.
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

{ #category : #initialization }
DRIRGenerator >> initialize [

	super initialize.
	specialCases := Dictionary new.
	self initializeSpecialCases.
	
	currentOperandStack := Stack new.
	nextRegister := 0.
	nextBlockID := 1.
	
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
	specialCases at: #<= put: #interpretLessOrEqualsComparisonWith:.
	specialCases at: #> put: #interpretGreaterComparisonWith:.
	specialCases at: #or: put: #interpretOrWith:.
	specialCases at: #and: put: #interpretAndWith:.

	"Arithmetics"
	specialCases at: #+ put: #interpretAddWith:.
	specialCases at: #- put: #interpretSubstractWith:.
	specialCases at: #* put: #interpretMultiplyWith:.
	specialCases at: #negated put: #interpretNegatedWith:.

	"Type coercions"
	specialCases at: #asUnsignedInteger put: #interpretAsUnsignedIntegerWith:.
	specialCases at: #sumInteger:withInteger:ifOverflow: put: #interpretIntegerSumWithOverflowWith:.

	"Memory accesses"
	specialCases at: #longAt: put: #interpretMachineWordAtWith:.
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
	specialCases at: #trueObj put: #interpretTrueObjectWith:.
	specialCases at: #falseObj put: #interpretFalseObjectWith:.
	
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
	self haltIf: [ anInstructionClass = DRCopy and: [ (aCollection first isKindOf: DRNullValue) ] ].
	instruction := anInstructionClass operands: aCollection result: self allocateTemporaryRegister.
	instruction origin: thisContext sender stack.
	^ instruction
]

{ #category : #factory }
DRIRGenerator >> instantiateNoResultInstruction: anInstructionClass operands: aCollection [ 
	
	| instruction |
	instruction := anInstructionClass operands: aCollection result: DRNoRegister new.
	instruction origin: thisContext sender stack.
	^ instruction
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
	aRBMessageNode receiver acceptVisitor: self.
	operand1 := self popOperand.
	aRBMessageNode arguments first acceptVisitor: self.
	operand2 := self popOperand.

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

	| operand1 operand2 |
	aRBMessageNode receiver acceptVisitor: self.
	operand1 := self popOperand.
	aRBMessageNode arguments first acceptVisitor: self.
	operand2 := self popOperand.

	^ self addInstructionWithNoResultFrom: aRBMessageNode instructionKind: anInstructionKind operands: { operand1 . operand2 }
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretEqualityComparisonWith: aRBMessageNode [
	
	^ self interpretCondition: aRBMessageNode instructionKind: DREqualsComparison
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretFalseObjectWith: aRBVariableNode [ 
	
	self pushOperand: DRFalseObject new
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfFalseWith: aRBMessageNode [

	| falseBranchBasicBlockOut startingBasicBlock conditionalJump falseBranchSuccessor joinInstruction |
	startingBasicBlock := self currentBasicBlock.

	aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self instantiate: DRConditionalJump operands: { self popOperand }.
	
	self currentBasicBlock endInstruction: conditionalJump.
	
	falseBranchSuccessor := self newBasicBlockDuring: [ 
			 aRBMessageNode arguments first acceptVisitor: self ].
	conditionalJump falseBranch: falseBranchSuccessor.
	falseBranchSuccessor addPredecessor: startingBasicBlock.
	falseBranchBasicBlockOut := self currentBasicBlock.

	"Merge point"
	self newBasicBlock.
	
	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	falseBranchBasicBlockOut hasFinalInstruction ifFalse: [
		falseBranchBasicBlockOut jumpTo: self currentBasicBlock.
	].

	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump trueBranch: self currentBasicBlock.

	joinInstruction := startingBasicBlock blockValue
		                   joinValue: falseBranchBasicBlockOut blockValue
		                   onIRGenerator: self
								from: { startingBasicBlock. falseBranchBasicBlockOut }.
	joinInstruction addToBlock: self currentBasicBlock.
	self pushOperand: joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfTrueWith: aRBMessageNode [

	| trueBranchBasicBlockOut startingBasicBlock joinInstruction conditionalJump trueBranchSuccessor |
	startingBasicBlock := self currentBasicBlock.

	aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self instantiate: DRConditionalJump operands: { self popOperand }.
	
	self currentBasicBlock endInstruction: conditionalJump.

	trueBranchSuccessor := self newBasicBlockDuring: [ 
			 aRBMessageNode arguments first acceptVisitor: self ].
	conditionalJump trueBranch: trueBranchSuccessor.
	trueBranchSuccessor addPredecessor: startingBasicBlock.
	trueBranchBasicBlockOut := self currentBasicBlock.

	"Merge point"
	self newBasicBlock.
	
	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	trueBranchBasicBlockOut hasFinalInstruction ifFalse: [
		trueBranchBasicBlockOut jumpTo: self currentBasicBlock.
	].
	
	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump falseBranch: self currentBasicBlock.

	joinInstruction := startingBasicBlock blockValue
		                   joinValue: trueBranchBasicBlockOut blockValue
		                   onIRGenerator: self
								from: { startingBasicBlock. trueBranchBasicBlockOut }.
	joinInstruction addToBlock: self currentBasicBlock.
	self pushOperand: joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfTrueifFalseWith: aRBMessageNode [

	| trueBranchBasicBlockOut falseBranchBasicBlockOut joinInstruction conditionalJump  |
	aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self
		                   instantiate: DRConditionalJump
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

	"Also, in case both branches are finished at this point, the rest of this method is dead code"
	(trueBranchBasicBlockOut hasFinalInstruction and: [ falseBranchBasicBlockOut hasFinalInstruction ])
		ifTrue: [ 
			self currentBasicBlock markAsDeadCode.
			 ].

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

	"join the temporary values modified by the blocks"
	self currentTemporaryFrame: ((trueBranchBasicBlockOut temporaryFrame keys
	                             ,
	                             falseBranchBasicBlockOut temporaryFrame keys)
		                            asSet collect: [ :e | 
		                            e
		                            -> ((trueBranchBasicBlockOut temporaryFrame
				                              at: e
				                              ifAbsent: [ DRNullValue new ])
				                             joinValue:
					                             (falseBranchBasicBlockOut temporaryFrame
						                              at: e
						                              ifAbsent: [ DRNullValue new ])
				                             onIRGenerator: self
				                             from: { 
						                             trueBranchBasicBlockOut.
						                             falseBranchBasicBlockOut }) ])
		                           asDictionary.
	currentBasicBlock temporaryFrame: self currentTemporaryFrame.
	self pushOperand: joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIntegerSumWithOverflowWith: aRBMessageNode [

	"Interpret the two numbers and add them up"
	| leftOperand rightOperand addResult startingBasicBlock conditionalJump overflowSuccessor overflowBlockOut joinInstruction |
	aRBMessageNode arguments first acceptVisitor: self.
	leftOperand := self popOperand.
	aRBMessageNode arguments second acceptVisitor: self.
	rightOperand := self popOperand.
	addResult := self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRAdd
		operands: { leftOperand . rightOperand }.

	startingBasicBlock := self currentBasicBlock.	
	conditionalJump := self instantiate: DROverflowJump operands: { addResult }.
	self currentBasicBlock endInstruction: conditionalJump.

	overflowSuccessor := self newBasicBlockDuring: [ 
			 aRBMessageNode arguments third acceptVisitor: self ].
	conditionalJump trueBranch: overflowSuccessor.
	overflowSuccessor addPredecessor: startingBasicBlock.
	overflowBlockOut := self currentBasicBlock.

	"Merge point"
	self newBasicBlock.
	
	"Could happen that the evaluated block had a non local return.
	In that case, this block should not arrive to this merge point
		=> do not add the jump"
	overflowSuccessor hasFinalInstruction ifFalse: [
		overflowSuccessor jumpTo: self currentBasicBlock.
	].
	
	self currentBasicBlock addPredecessor: startingBasicBlock.
	conditionalJump falseBranch: self currentBasicBlock.

	joinInstruction := addResult
		                   joinValue: overflowBlockOut blockValue
		                   onIRGenerator: self
								from: { startingBasicBlock. overflowBlockOut }.
	joinInstruction addToBlock: self currentBasicBlock.
	^ joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretLeftShiftWith: aRBMessageNode [ 

	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRLeftShift
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretLessOrEqualsComparisonWith: aRBMessageNode [ 

	^ self interpretCondition: aRBMessageNode instructionKind: DRLessOrEqualsComparison
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretMachineWordAtPutWith: aRBMessageNode [ 
	
	| operand1 operand2 |
	1halt.
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	
	^ self currentBasicBlock addInstruction: (DRStore operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretMachineWordAtWith: aRBMessageNode [ 
	
	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	
	^ self currentBasicBlock addInstruction: (DRLoad operands: { operand1 . operand2 })
]

{ #category : #API }
DRIRGenerator >> interpretMethod: aMethodAST receiver: aReceiver [

	^ self interpretMethod: aMethodAST receiver: aReceiver arguments: #()
]

{ #category : #API }
DRIRGenerator >> interpretMethod: aMethodAST receiver: aReceiver arguments: arguments [
	| returnValue |
	self pushFrameWithReceiver: aReceiver.
	aMethodAST argumentNames with: arguments do: [ :name :value |
		self temporaryAt: name put: value
	].
	aMethodAST acceptVisitor: self.
	"Fall through the exit block.
	If the method had an explicit return, it is already done, no need to do it again."
	currentBasicBlock hasFinalInstruction
		ifFalse: [ currentBasicBlock jumpTo: self currentFrame exitBasicBlock ].
	self popFrame.
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
	
	| elementsToPop |
	aRBMessageNode arguments first acceptVisitor: self.
	elementsToPop := self popOperand operand1 value.
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

	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (DRRightShift
		operands: { operand1 . operand2 }
		result: self allocateTemporaryRegister)
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretStackTopWith: aRBMessageNode [ 

	self pushOperand: self currentOperandStack top
]

{ #category : #'special-cases' }
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
DRIRGenerator >> interpretSubstractWith: aRBMessageNode [ 
	
	^ self interpretBinaryInstruction: aRBMessageNode instructionKind: DRSubstract
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretTrueObjectWith: aRBVariableNode [ 

	^ self addInstructionFrom: aRBVariableNode instructionKind: DRCopy operands: { DRTrueObject new }
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretUnaryInstruction: aRBMessageNode instructionKind: anInstructionKind [
	
	| operand1 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.

	^ self addInstructionFrom: aRBMessageNode instructionKind: anInstructionKind operands: { operand1 }
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

{ #category : #factory }
DRIRGenerator >> nextBlockID [

	| allocatedID |
	allocatedID := nextBlockID.
	nextBlockID := nextBlockID + 1.
	^ allocatedID
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
	
	| poppedFrame |
	currentBasicBlock := self currentFrame exitBasicBlock.
	currentOperandStack := currentBasicBlock mergeOperandStacksIn: self.
	poppedFrame := interpretationStack removeLast.

	currentBasicBlock addInstruction: poppedFrame returnValue.
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
DRIRGenerator >> pushFrameWithReceiver: aReceiver [
	
	| frame newMethodEntry |
	newMethodEntry := self basicNewBasicBlock.
	self currentBasicBlock jumpTo: newMethodEntry.
	currentBasicBlock := newMethodEntry.
	
	frame := DruidStackFrame new
		receiver: aReceiver;
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

{ #category : #'frame-access' }
DRIRGenerator >> temporaryAt: aString [ 
	
	^ self currentBasicBlock temporaryAt: aString
]

{ #category : #'frame-access' }
DRIRGenerator >> temporaryAt: aString put: aValue [ 

	^ self currentBasicBlock temporaryAt: aString put: aValue
]

{ #category : #visiting }
DRIRGenerator >> visitArgumentVariableNode: aRBVariableNode [ 
	
	^ self visitTemporaryVariableNode: aRBVariableNode 
]

{ #category : #visiting }
DRIRGenerator >> visitAssignmentNode: aRBAssignmentNode [ 
	
	aRBAssignmentNode value acceptVisitor: self.
	
	aRBAssignmentNode variable binding isTempVariable ifTrue: [ 
		^ self
			temporaryAt: aRBAssignmentNode variable name
			put: self operandStackTop ].
	
	aRBAssignmentNode variable binding isInstanceVariable
		ifTrue: [ 1halt.^ (self receiver value instVarNamed: aRBAssignmentNode variable name put: self stackTop) asDRInterpreterValue ].
	
	self halt
]

{ #category : #visiting }
DRIRGenerator >> visitBlockNode: aRBBlockNode [
	
	| blockValue instruction |
	aRBBlockNode statements do: [ :e |
		e acceptVisitor: self.
		e isReturn ifTrue: [ ^ self ].
		blockValue := self popOperand.
	].
	"At the end, transform the block value into a copy instruction"
	self
		addInstructionFrom: aRBBlockNode
		instructionKind: DRCopy
		operands: {blockValue}.
	
	"The add instruction pushes the instruction to the operand stack.
	Pop it: at the end of the block execution, nothing should be in the stack"
	self currentBasicBlock blockValue: self popOperand.

]

{ #category : #visiting }
DRIRGenerator >> visitInstanceVariableNode: aRBVariableNode [ 
	
	| receiver arguments method |
	specialCases
		at: aRBVariableNode name
		ifPresent: [ :mapping |
			^ self perform: mapping withArguments: { aRBVariableNode } ].
	
	"Resolve it as a variableAccess"
	self haltIf: [ (self receiver isKindOf: DRInterpreterValue) not ].
	^ DRInterpreterValue value: (self receiver value instVarNamed: aRBVariableNode name)
]

{ #category : #visiting }
DRIRGenerator >> visitLiteralValueNode: aRBLiteralValueNode [ 
	
	self addInstructionFrom: aRBLiteralValueNode instructionKind: DRCopy operands: { (DRConstantValue value: aRBLiteralValueNode value) }
]

{ #category : #visiting }
DRIRGenerator >> visitMessageNode: aRBMessageNode [ 

	| receiver arguments method |
	specialCases
		at: aRBMessageNode selector
		ifPresent: [ :mapping |
			^ self perform: mapping withArguments: { aRBMessageNode } ].
	
	"Resolve it as a normal send"
	receiver := aRBMessageNode receiver acceptVisitor: self.
	arguments := aRBMessageNode arguments collect: [ :e |
		e acceptVisitor: self.
		self popOperand ].
	aRBMessageNode isSuperSend ifTrue: [ self halt ].
	method := receiver lookupClass lookupSelector: aRBMessageNode selector.
	self interpretMethod: method ast receiver: receiver arguments: arguments
]

{ #category : #visiting }
DRIRGenerator >> visitMethodNode: aRBMethodNode [ 
	
	aRBMethodNode statements do: [ :each |
		each acceptVisitor: self.
		each isReturn ifTrue: [ ^ self ].
		self popOperand. ]
]

{ #category : #visiting }
DRIRGenerator >> visitReturnNode: aRBReturnNode [

	| result |
	aRBReturnNode value acceptVisitor: self.

	result := self popOperand copyToVariableIn: self inNode: aRBReturnNode.
	self currentFrame returnValue: result from: self currentBasicBlock.
	
	"If the block has no predecessors, do not add it any successors.
	Otherwise, when searching for the entire list of blocks, this block will never be visited,
	and it will avoid its successors to be visited (in reverse post order) too"
	self flag: #todo.
	self currentBasicBlock lastInstruction = #deadCode ifTrue: [ ^ self ].

	self currentBasicBlock jumpTo: self currentFrame exitBasicBlock.
]

{ #category : #visiting }
DRIRGenerator >> visitSelfNode: aRBVariableNode [ 
	
	^ self receiver
]

{ #category : #visiting }
DRIRGenerator >> visitTemporaryVariableNode: aRBVariableNode [ 
	
	^ self pushOperand: (self temporaryAt: aRBVariableNode name)
]
