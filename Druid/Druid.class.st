Class {
	#name : #Druid,
	#superclass : #Object,
	#instVars : [
		'vmInterpreter',
		'specialSelectorTable',
		'instructionStream',
		'currentInstruction',
		'irBuilder'
	],
	#category : #Druid
}

{ #category : #generator }
Druid class >> generateIRFromBytecode: bytecodeList [
	
	| vmInterpreter |
	vmInterpreter := self stackInterpreter.

	^ self new
		vmInterpreter: vmInterpreter;
		interpretBytecode: bytecodeList
]

{ #category : #accessing }
Druid class >> stackInterpreter [

	StackInterpreterSimulatorLSB
		initializeWithOptions: {
			#BytesPerWord. 8.
			#ObjectMemory. #Spur64BitMemoryManager.
			#bytecodeTableInitializer. #initializeBytecodeTableForSistaV1}
		objectMemoryClass: Spur64BitMemoryManager.
	^ StackInterpreterSimulatorLSB basicNew
		objectMemory: Spur64BitMemoryManager simulatorClass new;
		basicInitialize;
		internalizeIPandSP;
		yourself
]

{ #category : #'IR-building' }
Druid >> appendInstruction: anInstruction [

	instructions addLast: anInstruction
]

{ #category : #interpreting }
Druid >> fetchNextInstruction [

	currentInstruction := instructionStream next
]

{ #category : #initialization }
Druid >> initialize [

	super initialize.
	irBuilder := DRIRBuilder new.
	
	specialSelectorTable := Dictionary new.
	specialSelectorTable at: #fetchNextBytecode put: #interpretFetchNextBytecodeOn:receiver:arguments:.
	specialSelectorTable at: #longAt: put: #interpretLongAtOn:receiver:arguments:.
	specialSelectorTable at: #internalPush: put: #interpretInternalPushOn:receiver:arguments:.
	specialSelectorTable at: #+ put: #interpretSumOn:receiver:arguments:
]

{ #category : #interpreting }
Druid >> interpretAST: aRBMethodNode [
	
	^ self interpretAST: aRBMethodNode withArguments: #()
]

{ #category : #interpreting }
Druid >> interpretAST: aRBMethodNode withArguments: arguments [ 
	
	^ self
		interpretAST: aRBMethodNode
		withReceiver: vmInterpreter
		withArguments: arguments
]

{ #category : #interpreting }
Druid >> interpretAST: aRBMethodNode withReceiver: aReceiver withArguments: arguments [ 
	
	self assert: arguments isEmpty.
	^ aRBMethodNode acceptVisitor: self
]

{ #category : #interpreting }
Druid >> interpretBytecode: aCollection [ 
	
	instructionStream := aCollection readStream.
	self fetchNextInstruction.
	[ currentInstruction notNil ] whileTrue: [ | bytecodeSelector |
		bytecodeSelector := vmInterpreter class bytecodeTable at: currentInstruction + 1.
		self interpretAST: (self lookupSelector: bytecodeSelector)
	]
]

{ #category : #'special-cases' }
Druid >> interpretFetchNextBytecodeOn: aMessageSendNode receiver: aReceiver arguments: arguments [

	self fetchNextInstruction
]

{ #category : #'special-cases' }
Druid >> interpretInternalPushOn: aRBMessageNode receiver: aStackInterpreterSimulatorLSB arguments: aCollection [ 
	
	^ irBuilder push: aCollection first
]

{ #category : #'special-cases' }
Druid >> interpretLongAtOn: aRBMessageNode receiver: aReceiver arguments: aCollection [ 
	
	^ DRLoad value: aCollection first
]

{ #category : #'special-cases' }
Druid >> interpretSumOn: aRBMessageNode receiver: aReceiver arguments: arguments [
	
	"This is a sum +.
	Either both receiver and arguments are constants and we can fold them, only if they do not overflow.
	Otherwise, we need to load the values and sum them."
	self assert: arguments size = 1.
	
	(aReceiver isConstantInteger and: [ arguments first isConstantInteger ])
		ifTrue: [ ^ irBuilder newConstant: aReceiver value + arguments first value ].
	
	^ irBuilder add: aReceiver and: arguments first
]

{ #category : #interpreting }
Druid >> lookupSelector: aSelector [
	
	| foundMethod |
	foundMethod := vmInterpreter class lookupSelector: aSelector.
	foundMethod ifNil: [ self error: 'Selector ', aSelector, ' not found. Check if this needs to be implemented as a special case' ].
	^ foundMethod ast
]

{ #category : #visiting }
Druid >> visitGlobalNode: aRBGlobalNode [ 
	
	self assert: aRBGlobalNode binding value isNumber.
	^ irBuilder newConstant: aRBGlobalNode binding value
]

{ #category : #visiting }
Druid >> visitInstanceVariableNode: aRBInstanceVariableNode [ 
	
	aRBInstanceVariableNode name = #localFP
		ifTrue: [ ^ DRPhisicalRegister basePointer ].

	aRBInstanceVariableNode name = #stackPages
		ifTrue: [ ^ DRInvalid new ].

	^ self notYetImplemented
]

{ #category : #visiting }
Druid >> visitMessageNode: aRBMessageNode [ 
	
	| arguments astToInterpret receiver |
	"First interpret the arguments to generate instructions for them.
	If this is a special selector, treat it specially with those arguments.
	Otherwise, lookup and interpret the called method propagating the arguments"
	receiver := aRBMessageNode receiver acceptVisitor: self.
	arguments := aRBMessageNode arguments collect: [ :e | e acceptVisitor: self ].
	specialSelectorTable
		at: aRBMessageNode selector
		ifPresent: [ :selfSelectorToInterpret |
			^ self perform: selfSelectorToInterpret with: aRBMessageNode with: receiver with: arguments ].

	astToInterpret := self lookupSelector: aRBMessageNode selector.
	^ self interpretAST: astToInterpret withReceiver: receiver withArguments: arguments.
]

{ #category : #visiting }
Druid >> visitMethodNode: aRBMethodNode [ 

	| lastExpressionValue |
	lastExpressionValue := vmInterpreter.
	aRBMethodNode statements do: [ :statement |
		lastExpressionValue := statement acceptVisitor: self ].
	^ lastExpressionValue
]

{ #category : #visiting }
Druid >> visitReturnNode: aRBReturnNode [ 
	
	"For now, only return from the end of a method"
	self assert: aRBReturnNode methodNode statements last = aRBReturnNode.
	
	^ aRBReturnNode value acceptVisitor: self
]

{ #category : #visiting }
Druid >> visitSelfNode: aRBSelfNode [ 
	
	^ vmInterpreter
]

{ #category : #accessing }
Druid >> vmInterpreter [
	^ vmInterpreter
]

{ #category : #accessing }
Druid >> vmInterpreter: anObject [
	vmInterpreter := anObject
]
