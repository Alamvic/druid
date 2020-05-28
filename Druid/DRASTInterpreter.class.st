Class {
	#name : #DRASTInterpreter,
	#superclass : #Object,
	#instVars : [
		'vmInterpreter',
		'specialSelectorTable',
		'instructionStream',
		'irBuilder',
		'currentContext',
		'currentBytecode'
	],
	#category : #'Druid-Interpreter'
}

{ #category : #interpreting }
DRASTInterpreter >> activateBlock: aDRBlock [

	| lastValue |
	self
		pushNewContextWithMethod: aDRBlock block
		withReceiver: currentContext receiver
		withArguments: #().
	currentContext block: aDRBlock.

	lastValue := nil.
	aDRBlock block statements do: [ :e | 
		lastValue := e acceptVisitor: self ].
	
	self popContext.
	
	^ lastValue
]

{ #category : #'special-cases' }
DRASTInterpreter >> fetchNextInstruction [

	currentBytecode := instructionStream next
]

{ #category : #initialization }
DRASTInterpreter >> initialize [

	super initialize.
	irBuilder := DRIRBuilder new.
	
	specialSelectorTable := Dictionary new.
	specialSelectorTable at: #fetchNextBytecode put: #interpretFetchNextBytecodeOn:receiver:arguments:.
	specialSelectorTable at: #longAt: put: #interpretLongAtOn:receiver:arguments:.
	specialSelectorTable at: #internalPush: put: #interpretInternalPushOn:receiver:arguments:.

	"Arithmetic messages"
	specialSelectorTable at: #+ put: #interpretSumOn:receiver:arguments:.
	specialSelectorTable at: #* put: #interpretMultiplicationOn:receiver:arguments:.
	specialSelectorTable at: #anyMask: put: #interpretAnyMask:receiver:arguments:.
	specialSelectorTable at: #bitAnd: put: #interpretBitAnd:receiver:arguments:.
	specialSelectorTable at: #<< put: #interpretBitShift:receiver:arguments:.
	specialSelectorTable at: #bitShift: put: #interpretBitShift:receiver:arguments:.
	specialSelectorTable at: #negated put: #interpretNegated:receiver:arguments:.
	specialSelectorTable at: #- put: #interpretSubstract:receiver:arguments:.

	"Comparison"
	specialSelectorTable at: #= put: #interpretEquals:receiver:arguments:.

	"Control-flow"
	specialSelectorTable at: #ifTrue:ifFalse: put: #interpretIfTrueIfFalse:receiver:arguments:.	
	specialSelectorTable at: #cCode:inSmalltalk: put: #interpretcCodeInSmalltalk:receiver:arguments:.

]

{ #category : #interpreting }
DRASTInterpreter >> interpretAST: aRBMethodNode [
	
	^ self interpretAST: aRBMethodNode withArguments: #()
]

{ #category : #interpreting }
DRASTInterpreter >> interpretAST: aRBMethodNode withArguments: arguments [ 
	
	^ self
		interpretAST: aRBMethodNode
		withReceiver: vmInterpreter
		withArguments: arguments
]

{ #category : #interpreting }
DRASTInterpreter >> interpretAST: aRBMethodNode withReceiver: aReceiver withArguments: arguments [ 

	self pushNewContextWithMethod: aRBMethodNode withReceiver: aReceiver withArguments: arguments.
	[ ^ aRBMethodNode acceptVisitor: self ]
		ensure: [ self popContext ]
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretAnyMask: aRBMessageNode receiver: aReceiver arguments: arguments [ 
	
	"This is a logical bitAnd.
	Either both receiver and arguments are constants and we can fold them, only if they do not overflow.
	Otherwise, we need to load the values and sum them."
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: (aReceiver value anyMask: arguments first value) ].
	
	^ irBuilder bitAnd: aReceiver with: arguments first
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretBitAnd: aRBMessageNode receiver: aReceiver arguments: arguments [
	
	"This is a logical bitAnd.
	Either both receiver and arguments are constants and we can fold them, only if they do not overflow.
	Otherwise, we need to load the values and sum them."
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: (aReceiver value bitAnd: arguments first value) ].
	
	^ irBuilder bitAnd: aReceiver with: arguments first
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretBitShift: aRBMessageNode receiver: aReceiver arguments: arguments [ 
	
	"This is a bit shift.
	Either both receiver and arguments are constants and we can fold them.
	Otherwise, we need to load the values and sum them."
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: (aReceiver value bitShift: arguments first value) ].
	
	^ irBuilder bitShift: aReceiver magnitude: arguments first
]

{ #category : #interpreting }
DRASTInterpreter >> interpretBytecode: aCollection [ 
	
	instructionStream := aCollection readStream.
	self fetchNextInstruction.
	[ currentBytecode notNil ] whileTrue: [ | bytecodeSelector |
		bytecodeSelector := vmInterpreter class bytecodeTable at: currentBytecode + 1.
		self interpretAST: (self lookupSelector: bytecodeSelector receiver: vmInterpreter)
	]
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretEquals: aRBMessageNode receiver: aReceiver arguments: arguments [ 
	
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: (aReceiver value = arguments first value) asBit ].
	
	^ irBuilder compare: aReceiver with: arguments first
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretFetchNextBytecodeOn: aMessageSendNode receiver: aReceiver arguments: arguments [

	self fetchNextInstruction
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretIfTrueIfFalse: aRBMessageNode receiver: condition arguments: arguments [ 

	| jumpFalse jumpEnd |
	self assert: irBuilder instructions last canBeConditionalized.

	jumpFalse := irBuilder jumpFalse.
	self activateBlock: arguments first.
	jumpEnd := irBuilder jump.
	jumpFalse target: irBuilder label.
	self activateBlock: arguments second.
	jumpEnd target: irBuilder label.
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretInternalPushOn: aRBMessageNode receiver: aStackInterpreterSimulatorLSB arguments: aCollection [ 
	
	^ irBuilder push: aCollection first
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretLongAtOn: aRBMessageNode receiver: aReceiver arguments: aCollection [ 
	
	^ irBuilder loadAddress: aCollection first
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretMultiplicationOn: aRBMessageNode receiver: aReceiver arguments: arguments [ 
	
	"This is a multiplication.
	Either both receiver and arguments are constants and we can fold them.
	Otherwise, we need to load the values and sum them."
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: aReceiver value * arguments first value ].
	
	^ irBuilder multiply: aReceiver and: arguments first

]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretNegated: aRBMessageNode receiver: aReceiver arguments: arguments [ 
	
	self assert: arguments isEmpty.
	
	aReceiver isConstantInteger
		ifTrue: [ ^ irBuilder newConstant: aReceiver value negated ].
	
	^ irBuilder negate: aReceiver
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretSubstract: aRBMessageNode receiver: aReceiver arguments: arguments [ 
	
	"This is a substraction.
	Either both receiver and arguments are constants and we can fold them, only if they do not overflow.
	Otherwise, we need to load the values and sum them."
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: aReceiver value - arguments first value ].
	
	^ irBuilder substract: aReceiver and: arguments first
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretSumOn: aRBMessageNode receiver: aReceiver arguments: arguments [
	
	"This is a sum +.
	Either both receiver and arguments are constants and we can fold them, only if they do not overflow.
	Otherwise, we need to load the values and sum them."
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: aReceiver value + arguments first value ].
	
	^ irBuilder add: aReceiver and: arguments first
]

{ #category : #'special-cases' }
DRASTInterpreter >> interpretcCodeInSmalltalk: messageNode receiver: receiver arguments: arguments [

	^ self activateBlock: arguments second
	

]

{ #category : #accessing }
DRASTInterpreter >> irBuilder [
	^ irBuilder
]

{ #category : #accessing }
DRASTInterpreter >> irBuilder: aDRIRBuilder [ 
	irBuilder := aDRIRBuilder
]

{ #category : #interpreting }
DRASTInterpreter >> lookupSelector: aSelector receiver: aReceiver [
	
	| foundMethod |
	foundMethod := aReceiver class lookupSelector: aSelector.
	foundMethod ifNil: [ self error: 'Selector ', aSelector, ' not found. Check if this needs to be implemented as a special case' ].
	^ foundMethod ast
]

{ #category : #interpreting }
DRASTInterpreter >> lookupSelector: aSelector receiver: aReceiver isSuper: isSuper [
	
	| foundMethod classToLookup |
	
	classToLookup := isSuper
		ifTrue: [ currentContext method methodClass superclass ]
		ifFalse: [ aReceiver class ].
	
	foundMethod := classToLookup lookupSelector: aSelector.
	foundMethod ifNil: [ self error: 'Selector ', aSelector, ' not found. Check if this needs to be implemented as a special case' ].
	^ foundMethod ast
]

{ #category : #interpreting }
DRASTInterpreter >> popContext [
	
	currentContext := currentContext parent
]

{ #category : #interpreting }
DRASTInterpreter >> pushNewContextWithMethod: aRBMethodNode withReceiver: aReceiver withArguments: arguments [
	
	| newContext |
	newContext := DRInterpreterContext new.
	newContext parent: currentContext.
	newContext method: aRBMethodNode.
	newContext receiver: aReceiver.
	newContext arguments: arguments.
	currentContext := newContext
]

{ #category : #accessing }
DRASTInterpreter >> receiver [
	
	^ currentContext receiver
]

{ #category : #visiting }
DRASTInterpreter >> visitArgumentNode: aRBArgumentNode [ 
	
	^ currentContext argumentNamed: aRBArgumentNode name
]

{ #category : #visiting }
DRASTInterpreter >> visitAssignmentNode: aRBAssignmentNode [ 
	
	| value |
	
	value := aRBAssignmentNode value acceptVisitor: self.
	
	currentContext tempNamed: aRBAssignmentNode variable name put: value.
	^ value
]

{ #category : #visiting }
DRASTInterpreter >> visitBlockNode: aRBBlockNode [ 
	
	^ DRBlock new
		block: aRBBlockNode;
		home: currentContext;
		yourself
]

{ #category : #visiting }
DRASTInterpreter >> visitGlobalNode: aRBGlobalNode [ 
	
	self assert: aRBGlobalNode binding value isNumber.
	^ irBuilder newConstant: aRBGlobalNode binding value
]

{ #category : #visiting }
DRASTInterpreter >> visitInstanceVariableNode: aRBInstanceVariableNode [ 
	
	aRBInstanceVariableNode name = #localFP
		ifTrue: [ ^ DRPhysicalRegister basePointer ].

	aRBInstanceVariableNode name = #localSP
		ifTrue: [ ^ DRPhysicalRegister stackPointer ].

	aRBInstanceVariableNode name = #currentBytecode
		ifTrue: [ ^ irBuilder newConstant: currentBytecode ].

	"By default, fetch the instance variable of the receiver"
	^ self receiver instVarNamed: aRBInstanceVariableNode name
]

{ #category : #visiting }
DRASTInterpreter >> visitLiteralValueNode: aRBLiteralValueNode [ 
	
	^ irBuilder newConstant: aRBLiteralValueNode value
]

{ #category : #visiting }
DRASTInterpreter >> visitMessageNode: aRBMessageNode [ 
	
	| arguments astToInterpret receiver |
	
	"If this is an assertion, fast skip and do not look inside"
	aRBMessageNode selector = #assert: 
		ifTrue: [ ^ self ].
	
	"First interpret the arguments to generate instructions for them.
	If this is a special selector, treat it specially with those arguments.
	Otherwise, lookup and interpret the called method propagating the arguments"
	receiver := aRBMessageNode receiver acceptVisitor: self.
	arguments := aRBMessageNode arguments collect: [ :e | e acceptVisitor: self ].

	specialSelectorTable
		at: aRBMessageNode selector
		ifPresent: [ :selfSelectorToInterpret |
			^ self perform: selfSelectorToInterpret with: aRBMessageNode with: receiver with: arguments ].

	astToInterpret := self
		lookupSelector: aRBMessageNode selector
		receiver: receiver
		isSuper: aRBMessageNode receiver isSuper.
	^ self interpretAST: astToInterpret withReceiver: receiver withArguments: arguments.
]

{ #category : #visiting }
DRASTInterpreter >> visitMethodNode: aRBMethodNode [ 

	| lastExpressionValue |
	lastExpressionValue := vmInterpreter.
	aRBMethodNode statements do: [ :statement |
		lastExpressionValue := statement acceptVisitor: self ].
	^ lastExpressionValue
]

{ #category : #visiting }
DRASTInterpreter >> visitReturnNode: aRBReturnNode [ 
	
	"For now, only return from the end of a method"
	self assert: aRBReturnNode methodNode statements last = aRBReturnNode.
	
	^ aRBReturnNode value acceptVisitor: self
]

{ #category : #visiting }
DRASTInterpreter >> visitSelfNode: aRBSelfNode [ 
	
	^ self receiver
]

{ #category : #visiting }
DRASTInterpreter >> visitSuperNode: aRBSuperNode [ 
	
	^ self receiver
]

{ #category : #visiting }
DRASTInterpreter >> visitTemporaryNode: aRBTemporaryNode [ 
	
	^ currentContext temporaryNamed: aRBTemporaryNode name
]

{ #category : #accessing }
DRASTInterpreter >> vmInterpreter [
	^ vmInterpreter
]

{ #category : #accessing }
DRASTInterpreter >> vmInterpreter: anObject [
	vmInterpreter := anObject
]
