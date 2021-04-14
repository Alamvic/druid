Class {
	#name : #DRIRGenerator,
	#superclass : #Object,
	#instVars : [
		'specialCases',
		'interpretationStack',
		'currentBasicBlock',
		'firstBasicBlock',
		'numberOfArguments',
		'nextRegister'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #registers }
DRIRGenerator >> allocateTemporaryRegister [
	
	| allocatedRegisterId |
	allocatedRegisterId := nextRegister.
	nextRegister := nextRegister + 1.
	^ DRTemporaryRegister id: allocatedRegisterId
]

{ #category : #registers }
DRIRGenerator >> arg0register [
	
	^ self argRegisterNumber: 0
]

{ #category : #registers }
DRIRGenerator >> argRegisterNumber: anInteger [

	| argumentIndex |
	numberOfArguments = anInteger
		ifTrue: [ ^ self receiverRegister ].
	argumentIndex := numberOfArguments - anInteger - 1.
	^ DRPhysicalRegister name: 'Arg', argumentIndex asString, 'Reg'
]

{ #category : #accessing }
DRIRGenerator >> currentBasicBlock [
	^ currentBasicBlock
]

{ #category : #accessing }
DRIRGenerator >> currentFrame [
	
	^ interpretationStack last
]

{ #category : #initialization }
DRIRGenerator >> initialize [

	super initialize.
	specialCases := Dictionary new.
	self initializeSpecialCases.
	
	nextRegister := 0.
	interpretationStack := OrderedCollection new.
	
	firstBasicBlock := self newBasicBlock
]

{ #category : #'special-cases' }
DRIRGenerator >> initializeSpecialCases [

	"Control flow and comparisons"
	specialCases at: #ifTrue:ifFalse: put: #interpretIfTrueifFalseWith:.
	specialCases at: #ifTrue: put: #interpretIfTrueWith:.
	specialCases at: #ifFalse: put: #interpretIfFalseWith:.
	specialCases at: #= put: #interpretEqualityComparisonWith:.
	specialCases at: #<= put: #interpretLessOrEqualsComparisonWith:.

	"Arithmetics"
	specialCases at: #+ put: #interpretAddWith:.
	specialCases at: #- put: #interpretSubstractWith:.
	specialCases at: #* put: #interpretMultiplyWith:.
	specialCases at: #negated put: #interpretNegatedWith:.

	"Type coercions"
	specialCases at: #asUnsignedInteger put: #interpretAsUnsignedIntegerWith:.

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
DRIRGenerator >> instantiate: anInstructionClass operands: aCollection [ 
	
	^ anInstructionClass operands: aCollection result: self allocateTemporaryRegister
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretAddWith: aRBMessageNode [ 

	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (self instantiate: DRAdd operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretAnyMaskWith: aRBMessageNode [ 

	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (self instantiate: DRTest operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretAsUnsignedIntegerWith: aRBMessageNode [ 
	
	| operand1 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (DRUnsignedCoercion operands: { operand1 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretBitAndWith: aRBMessageNode [
	
	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (self instantiate: DRBitAnd operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretBitShiftWith: aRBMessageNode [ 

	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (self instantiate: DRBitShift operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretCCodeInSmalltalkWith: aRBMessageNode [ 
	
	^ aRBMessageNode arguments first acceptVisitor: self
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretEqualityComparisonWith: aRBMessageNode [
	
	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	
	^ self currentBasicBlock addInstruction: (self instantiate: DREqualsComparison operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretFalseObjectWith: aRBVariableNode [ 
	
	^ DRFalseObject new
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfFalseWith: aRBMessageNode [

	| falseBranchBasicBlockOut condition startingBasicBlock |
	startingBasicBlock := self currentBasicBlock.

	condition := aRBMessageNode receiver acceptVisitor: self.
	condition falseBranch: (self newBasicBlockDuring: [ 
			 aRBMessageNode arguments first acceptVisitor: self ]).
	falseBranchBasicBlockOut := self currentBasicBlock.

	self newBasicBlock.
	falseBranchBasicBlockOut jumpTo: self currentBasicBlock.
	condition trueBranch: self currentBasicBlock.

	self currentBasicBlock addInstruction:
		(startingBasicBlock blockValue
			 joinValue: falseBranchBasicBlockOut blockValue
			 onIRGenerator: self
			 from: { startingBasicBlock. falseBranchBasicBlockOut}).
	^ self currentBasicBlock lastInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfTrueWith: aRBMessageNode [

	| trueBranchBasicBlockOut condition startingBasicBlock joinInstruction conditionalJump |
	startingBasicBlock := self currentBasicBlock.

	condition := aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self instantiate: DRConditionalJump operands: { condition }.
	
	self currentBasicBlock endInstruction: conditionalJump.

	conditionalJump trueBranch: (self newBasicBlockDuring: [ 
			 aRBMessageNode arguments first acceptVisitor: self ]).
	trueBranchBasicBlockOut := self currentBasicBlock.

	self newBasicBlock.
	trueBranchBasicBlockOut jumpTo: self currentBasicBlock.
	conditionalJump falseBranch: self currentBasicBlock.

	joinInstruction := startingBasicBlock blockValue
		                   joinValue: trueBranchBasicBlockOut blockValue
		                   onIRGenerator: self
								from: { startingBasicBlock. trueBranchBasicBlockOut }.
	joinInstruction addToBlock: self currentBasicBlock.
	^ joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretIfTrueifFalseWith: aRBMessageNode [

	| trueBranchBasicBlockOut falseBranchBasicBlockOut condition joinInstruction temporaryFrameBeforeBranch temporaryFrameAfterTrueBranchJoin temporaryFrameAfterFalseBranchJoin joinPointTemporartFrame conditionalJump |
	condition := aRBMessageNode receiver acceptVisitor: self.
	conditionalJump := self instantiate: DRConditionalJump operands: { condition }.
	self currentBasicBlock endInstruction: conditionalJump.
	
	temporaryFrameBeforeBranch := self currentFrame temporaryFrame copy.
	conditionalJump trueBranch: (self newBasicBlockDuring: [ 
			 aRBMessageNode arguments first acceptVisitor: self ]).
	trueBranchBasicBlockOut := self currentBasicBlock.
	temporaryFrameAfterTrueBranchJoin := self currentFrame temporaryFrame.

	self currentFrame temporaryFrame: temporaryFrameBeforeBranch.
	conditionalJump falseBranch: (self newBasicBlockDuring: [ 
			 aRBMessageNode arguments second acceptVisitor: self ]).
	falseBranchBasicBlockOut := self currentBasicBlock.
	temporaryFrameAfterFalseBranchJoin := self currentFrame
		                                      temporaryFrame.

	self newBasicBlock.
	trueBranchBasicBlockOut jumpTo: self currentBasicBlock.
	falseBranchBasicBlockOut jumpTo: self currentBasicBlock.

	"join the values returned by the blocks"
	joinInstruction := trueBranchBasicBlockOut blockValue
		                   joinValue: falseBranchBasicBlockOut blockValue
		                   onIRGenerator: self
		                   from: { 
				                   trueBranchBasicBlockOut.
				                   falseBranchBasicBlockOut }.

	"join the temporary values modified by the blocks"
	joinPointTemporartFrame := ((temporaryFrameAfterTrueBranchJoin keys
	                             ,
	                             temporaryFrameAfterFalseBranchJoin keys)
		                            asSet collect: [ :e | 
		                            e
		                            -> ((temporaryFrameAfterTrueBranchJoin
				                              at: e
				                              ifAbsent: [ DRNullValue new ])
				                             joinValue:
					                             (temporaryFrameAfterFalseBranchJoin
						                              at: e
						                              ifAbsent: [ DRNullValue new ])
				                             onIRGenerator: self
				                             from: { 
						                             trueBranchBasicBlockOut.
						                             falseBranchBasicBlockOut }) ])
		                           asDictionary.
	self currentFrame temporaryFrame: joinPointTemporartFrame.

	^ joinInstruction
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretLeftShiftWith: aRBMessageNode [ 

	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (DRLeftShift operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretLessOrEqualsComparisonWith: aRBMessageNode [ 

	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	
	^ self currentBasicBlock addInstruction: (self instantiate: DRLessOrEqualsComparison operands: { operand1 . operand2 })
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
	
	self pushFrameWithReceiver: aReceiver.
	aMethodAST argumentNames with: arguments do: [ :name :value |
		self temporaryAt: name put: value
	].
	aMethodAST acceptVisitor: self.
	^ self popFrame.
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

	| operand1 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (self instantiate: DRNegate operands: { operand1 })
]

{ #category : #visiting }
DRIRGenerator >> interpretNotWith: aRBMessageNode [ 
	self shouldBeImplemented.
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretPopThenPushWith: aRBMessageNode [ 

	"Pop N elements from the operand stack.
	Pushes the second argument to the stack.
	
	Pop: keep track of the operand stack and remove one element
	Push: move the value to the physical register assigned to the current stack position"
	
	| value poppedElements |
	poppedElements := aRBMessageNode arguments first acceptVisitor: self.
	poppedElements operands first value isInteger
		ifFalse: [ self error: 'expected an integer value' ].
	
	self pop: poppedElements operands first value.
	^ self push: (aRBMessageNode arguments second acceptVisitor: self)
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

	^ self argRegisterNumber: 0
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretStackValueWith: aRBMessageNode [

	"Interpret messages of the form 
	interpreter stackValue: xxx.
	"	
	| value |
	value := aRBMessageNode arguments first acceptVisitor: self.
	value operand1 value isInteger ifFalse: [ self error: 'expected an integer value' ].
	
	^ self currentBasicBlock addInstruction: (self instantiate: DRCopy operands: {
		self argRegisterNumber: value operand1 value })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretSubstractWith: aRBMessageNode [ 
	
	| operand1 operand2 |
	operand1 := aRBMessageNode receiver acceptVisitor: self.
	operand2 := aRBMessageNode arguments first acceptVisitor: self.
	^ self currentBasicBlock addInstruction: (self instantiate: DRSubstract operands: { operand1 . operand2 })
]

{ #category : #'special-cases' }
DRIRGenerator >> interpretTrueObjectWith: aRBVariableNode [ 
	
	| copyInstruction |
	copyInstruction := self
		instantiate: DRCopy
		operands: { DRTrueValue new }.
	^ self currentBasicBlock addInstruction: copyInstruction
]

{ #category : #accessing }
DRIRGenerator >> ir [
	^ self subclassResponsibility
]

{ #category : #'basic-blocks' }
DRIRGenerator >> newBasicBlock [
	
	^ currentBasicBlock := DRBasicBlock new
]

{ #category : #'basic-blocks' }
DRIRGenerator >> newBasicBlockDuring: aFullBlockClosure [ 
	
	| inBasicBlock |
	inBasicBlock := self newBasicBlock.
	aFullBlockClosure value.
	^ inBasicBlock
]

{ #category : #'stack-management' }
DRIRGenerator >> popFrame [
	
	^ interpretationStack removeLast returnValue
]

{ #category : #'stack-management' }
DRIRGenerator >> pushFrameWithReceiver: aReceiver [
	
	| frame |
	frame := DruidStackFrame new
		receiver: aReceiver;
		irGenerator: self;
		exitBasicBlock: DRBasicBlock new;
		yourself.
	
	interpretationStack addLast: frame
]

{ #category : #accessing }
DRIRGenerator >> receiver [
	
	^ self currentFrame receiver
]

{ #category : #registers }
DRIRGenerator >> receiverRegister [
	
	^ DRPhysicalRegister name: 'ReceiverResultReg'
]

{ #category : #accessing }
DRIRGenerator >> temporaryAt: aString [ 
	
	^ self currentFrame temporaryAt: aString
]

{ #category : #interpretation }
DRIRGenerator >> temporaryAt: aString put: aValue [ 
	
	1haltIf: [ (aValue isKindOf: DRValue) not ].
	^ self currentFrame temporaryAt: aString put: aValue
]

{ #category : #visiting }
DRIRGenerator >> visitArgumentVariableNode: aRBVariableNode [ 
	
	^ self visitTemporaryVariableNode: aRBVariableNode 
]

{ #category : #visiting }
DRIRGenerator >> visitAssignmentNode: aRBAssignmentNode [ 
	
	| valueContainer |
	valueContainer := aRBAssignmentNode value acceptVisitor: self.
	
	aRBAssignmentNode variable binding isTempVariable
		ifTrue: [ ^ self temporaryAt: aRBAssignmentNode variable name put: valueContainer ].
	
	aRBAssignmentNode variable binding isInstanceVariable
		ifTrue: [ 1halt.^ (self receiver value instVarNamed: aRBAssignmentNode variable name put: valueContainer) asDRInterpreterValue ].
	
	self halt
]

{ #category : #visiting }
DRIRGenerator >> visitBlockNode: aRBBlockNode [
	
	aRBBlockNode statements do: [ :e |
		self currentBasicBlock blockValue: (e acceptVisitor: self) ].
	^ self currentBasicBlock blockValue
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
	
	| copyInstruction |
	copyInstruction := self
		instantiate: DRCopy
		operands: { DRConstantValue value: aRBLiteralValueNode value }.
	^ self currentBasicBlock addInstruction: copyInstruction
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
	arguments := aRBMessageNode arguments collect: [ :e | e acceptVisitor: self ].
	aRBMessageNode isSuperSend ifTrue: [ self halt ].
	method := receiver lookupClass lookupSelector: aRBMessageNode selector.
	^ self interpretMethod: method ast receiver: receiver arguments: arguments
]

{ #category : #visiting }
DRIRGenerator >> visitMethodNode: aRBMethodNode [ 
	
	aRBMethodNode statements do: [ :each |
		each acceptVisitor: self ]
]

{ #category : #visiting }
DRIRGenerator >> visitReturnNode: aRBReturnNode [

	| newReturn |
	newReturn := aRBReturnNode value acceptVisitor: self.

	self currentFrame returnValue: newReturn.

	self currentBasicBlock jumpTo: self currentFrame exitBasicBlock.
	currentBasicBlock := self currentFrame exitBasicBlock.
	self currentBasicBlock addInstruction: self currentFrame returnValue.
	
	^ DRNullValue new
]

{ #category : #visiting }
DRIRGenerator >> visitSelfNode: aRBVariableNode [ 
	
	^ self receiver
]

{ #category : #visiting }
DRIRGenerator >> visitTemporaryVariableNode: aRBVariableNode [ 
	
	^ self temporaryAt: aRBVariableNode name
]
