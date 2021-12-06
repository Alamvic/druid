Class {
	#name : #DRBasicBlock,
	#superclass : #Object,
	#instVars : [
		'id',
		'instructions',
		'blockValue',
		'endInstruction',
		'predecessors',
		'origin',
		'temporaryFrame',
		'vmOperandStack',
		'primitiveFailureCode',
		'controlFlowGraph',
		'isExitBlock'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRBasicBlock class >> id: anInteger [ 
	
	^ self new
		id: anInteger;
		yourself
]

{ #category : #building }
DRBasicBlock >> add: aValue to: anotherValue [
	
	^ self addInstruction: (self instructionFactory add: aValue to: anotherValue)
]

{ #category : #accessing }
DRBasicBlock >> addInstruction: anInstruction [

	anInstruction basicBlock: self.
	^ instructions add: anInstruction
]

{ #category : #accessing }
DRBasicBlock >> addPredecessor: aBasicBlock [

	predecessors add: aBasicBlock
]

{ #category : #transformations }
DRBasicBlock >> basicDuplicate [
	
	| instructionCopy |
	^ controlFlowGraph newBasicBlockWith: [ :newBlock | | oldToNew |
		oldToNew := Dictionary new.
		self predecessors do: [ :p | 
			newBlock addPredecessor: p ].

		"Duplicate instructions"
		self instructions allButLast do: [ :instruction |
			oldToNew at: instruction put: (instructionCopy := instruction copy).
			instructionCopy operands do: [ :e |
				e addUser: instructionCopy.
			].
			newBlock addInstruction: instructionCopy.
		 ].
		oldToNew at: self endInstruction put: (instructionCopy := self endInstruction copy).
		newBlock endInstruction: instructionCopy.
		newBlock endInstruction operands do: [ :e |
				e addUser: newBlock endInstruction.
		].

		"Rewrite all operands"
		newBlock instructions do: [ :newInstruction |
			newInstruction users copy do: [ :user | | replacement |
				"If an user comes from another block, keep using it"
				replacement := oldToNew at: user ifAbsent: [ user ].
				newInstruction removeUser: user.
				newInstruction addUser: replacement.
			].
			newInstruction operands copy do: [ :operand | | replacement |
				"If an operand comes from another block, keep using it"
				replacement := oldToNew at: operand ifAbsent: [ operand ].
				newInstruction replaceOperand: operand by: replacement.
			]
		].
	]	
]

{ #category : #accessing }
DRBasicBlock >> basicEndInstruction: aDRInstruction [ 

	aDRInstruction basicBlock: self.
	endInstruction := aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> basicRemovePredecessor: aDRBasicBlock [ 
	
	predecessors remove: aDRBasicBlock
]

{ #category : #accessing }
DRBasicBlock >> beExitBlock [

	isExitBlock := true
]

{ #category : #accessing }
DRBasicBlock >> blockValue [
	
	^ blockValue
]

{ #category : #accessing }
DRBasicBlock >> blockValue: aValue [
	
	self haltIf: [ (aValue isKindOf: DRValue) not ].
	blockValue := aValue
]

{ #category : #validating }
DRBasicBlock >> checkLivenessOf: aValue [
	
	"Check if the value exists within this branch of execution"
	"It should be defined by myself, or by a predecessor"
	
	(instructions includes: aValue) ifTrue: [ ^ true ].
	"Could check a predecessor multiple times... Less optimal but correct"
	"Does not handle cycles yet"
	^ self predecessors anySatisfy: [ :e | e checkLivenessOf: aValue ]
]

{ #category : #accessing }
DRBasicBlock >> controlFlowGraph [

	^ controlFlowGraph
]

{ #category : #accessing }
DRBasicBlock >> controlFlowGraph: anObject [

	controlFlowGraph := anObject
]

{ #category : #building }
DRBasicBlock >> copy: aValue [
	
	^ self addInstruction: (self instructionFactory copy: aValue)
]

{ #category : #'control-flow' }
DRBasicBlock >> duplicatePredecessor: existingPredecessor from: newPredecessor [
	
	"Add newPredecessor as predecessor, make sure all Phi functions in this block get a new value.
	The value should be the same as the one coming from the existing predecessor.
	
	Assumption: newPredecessor is a predecessor of existingPredecessor"
	
	self addPredecessor: newPredecessor.
	self phiFunctions do: [ :e |
		e addValue: (e operandAt: (self indexOfPredecessor: existingPredecessor)) ].
]

{ #category : #accessing }
DRBasicBlock >> endInstruction [

	^ endInstruction
]

{ #category : #accessing }
DRBasicBlock >> endInstruction: aDRInstruction [ 
	| previousSuccessors newSuccessors lostSuccessors |	
	previousSuccessors := self successors.
	newSuccessors := aDRInstruction targets.
	lostSuccessors := previousSuccessors difference: newSuccessors.
	
	lostSuccessors do: [ :e |
		e removePredecessor: self. ].

	self basicEndInstruction: aDRInstruction
]

{ #category : #building }
DRBasicBlock >> equals: operand1 to: operand2 [ 
	
	^ self addInstruction: (self instructionFactory equals: operand1 to: operand2)
]

{ #category : #accessing }
DRBasicBlock >> firstInstruction [
	
	^ instructions first
]

{ #category : #testing }
DRBasicBlock >> hasFinalInstruction [
	
	^ endInstruction notNil and: [endInstruction isNoop not]
]

{ #category : #testing }
DRBasicBlock >> hasPhiFunctions [

	^ instructions anySatisfy: [ :e | e isPhiFunction ]
]

{ #category : #testing }
DRBasicBlock >> hasPredecessor: aDRBasicBlock [ 
	
	^ predecessors includes: aDRBasicBlock
]

{ #category : #testing }
DRBasicBlock >> hasPredecessors [
	
	^ predecessors notEmpty
]

{ #category : #accessing }
DRBasicBlock >> hyperblock [

	| workingQueue visited visitsToThisBlockSoFar blocks |
	visited := Dictionary new.
	blocks := OrderedCollection new.
	workingQueue := OrderedCollection new.
	workingQueue add: self.
	"Assume I've been alredy been visited.
	This is to guarantee I will be processed regardless my predecessors"
	visited
		at: self
		put: self numberOfPredecessors -1.

	[ workingQueue isEmpty ] whileFalse: [ 
		| currentBlock |
		currentBlock := workingQueue removeFirst.

		"Count how many times we have visited this block"
		visitsToThisBlockSoFar := visited
			                          at: currentBlock
			                          ifPresent: [ :p | p ]
			                          ifAbsent: [ 0 ].

		visited at: currentBlock put: visitsToThisBlockSoFar + 1.
		visitsToThisBlockSoFar + 1 = currentBlock numberOfPredecessors
			ifTrue: [
				
				blocks add: currentBlock.

				(workingQueue isEmpty and: [
					blocks size > 1 and: [ blocks size = visited size ] ])
						ifTrue: [ ^ DRHyperBlock blocks: blocks ].
				
				currentBlock hasFinalInstruction ifTrue: [ 
					currentBlock lastInstruction targets do: [ :target | 
						target isExitBlock ifFalse: [ workingQueue add: target ]  ] ] ] ].

	^ DRHyperBlock blocks: blocks
]

{ #category : #accessing }
DRBasicBlock >> id [

	^ id
]

{ #category : #accessing }
DRBasicBlock >> id: anObject [

	id := anObject
]

{ #category : #accessing }
DRBasicBlock >> indexOfPredecessor: aDRBasicBlock [ 
	
	^ predecessors indexOf: aDRBasicBlock
]

{ #category : #accessing }
DRBasicBlock >> initialize [
	super initialize.	
	instructions := OrderedCollection new.
	blockValue := DRNullValue new.
	predecessors := OrderedCollection new.
	isExitBlock := false.

	endInstruction := DRNoop new.
	endInstruction basicBlock: self.
]

{ #category : #'as yet unclassified' }
DRBasicBlock >> inspectionInstructions: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Instructions'> 
	
	^ aBuilder newList
		display: [ :each | StObjectPrinter asTruncatedTextFrom: each prettyPrint ];
		items: self instructions;
		yourself
]

{ #category : #inspection }
DRBasicBlock >> inspectionSourceCode [
	<inspectorPresentationOrder: 30 title: 'Source code'>

	| sourceInterval |
	sourceInterval := self sourceInterval.
	^ SpCodePresenter new 
		beForBehavior: self methodNode methodClass;
		text: self source;
		addTextSegmentDecoration: (SpTextPresenterDecorator forHighlight
			interval: (sourceInterval first to: sourceInterval last + 1);
			yourself);
		yourself
]

{ #category : #building }
DRBasicBlock >> instructionFactory [
	
	^ controlFlowGraph instructionFactory
]

{ #category : #accessing }
DRBasicBlock >> instructions [

	| allInstructions |
	allInstructions := OrderedCollection new.
	self instructionsDo: [ :i | allInstructions add: i ].
	^ allInstructions
]

{ #category : #iterating }
DRBasicBlock >> instructionsDo: aFullBlockClosure [ 

	instructions do: aFullBlockClosure.
	endInstruction ifNotNil: [ aFullBlockClosure value: endInstruction ]
]

{ #category : #accessing }
DRBasicBlock >> isExitBlock [

	^ isExitBlock
]

{ #category : #accessing }
DRBasicBlock >> isExitBlock: anObject [

	isExitBlock := anObject
]

{ #category : #building }
DRBasicBlock >> jumpIf: condition to: aDRBasicBlock ifFalseTo: aDRBasicBlock2 [ 
	
	aDRBasicBlock addPredecessor: self.
	aDRBasicBlock2 addPredecessor: self.

	^ endInstruction := DRConditionalJump new
		operands: { condition };
		trueBranch: aDRBasicBlock;
		falseBranch: aDRBasicBlock2;
		yourself
]

{ #category : #building }
DRBasicBlock >> jumpIfTrueTo: aDRBasicBlock ifFalseTo: aDRBasicBlock2 [ 
	
	aDRBasicBlock addPredecessor: self.
	aDRBasicBlock2 addPredecessor: self.
	
	^ endInstruction := DRConditionalJump new
		trueBranch: aDRBasicBlock;
		falseBranch: aDRBasicBlock2;
		yourself
]

{ #category : #building }
DRBasicBlock >> jumpTo: aBasicBlock [ 
	
	self endInstruction: (DRJump new target: aBasicBlock).
	aBasicBlock addPredecessor: self.
	^ endInstruction
]

{ #category : #accessing }
DRBasicBlock >> lastInstruction [
	
	^ endInstruction
]

{ #category : #'operand stack' }
DRBasicBlock >> mergeOperandStacksIn: aBuilder [

	"We can only merge two operand stacks with the same number of elements"
	| stackSize newStack predecessorStacks phi predecessorFailures mergedPrimitiveFailure |
	
	"If I have no predecessors, I'm very probably dead code.
	No need to merge anything"
	self predecessors ifEmpty: [ ^ self ].
	
	"If I have a single predecessor, just copy the operand stack"
	self predecessors size = 1 ifTrue: [
		primitiveFailureCode := self predecessors first primitiveFailureCode.
		^ vmOperandStack := self predecessors first vmOperandStack copy ].

	predecessorFailures := self predecessors collect: [ :e | e primitiveFailureCode ].
	mergedPrimitiveFailure := (DRPhiFunction joinedVariables: predecessorFailures)
		result: aBuilder allocateTemporaryRegister;
		origin: thisContext stack.
	primitiveFailureCode := mergedPrimitiveFailure.
	mergedPrimitiveFailure isPhiFunction ifTrue: [ self addInstruction: mergedPrimitiveFailure ].

	predecessorStacks := self predecessors collect: [ :e | e vmOperandStack ].	
	stackSize := predecessorStacks max: [ :e | e size ].
	newStack := Stack new.
	1 to: stackSize do: [ :e | | values |
		values := predecessorStacks collect: [ :stack | 
			stack size >= e
				ifTrue: [ stack at: e ]
				ifFalse: [ DRNullValue new ] ].
		phi := (DRPhiFunction joinedVariables: values)
			result: aBuilder allocateTemporaryRegister;
			origin: thisContext stack;
			yourself.
		self haltIf: [ phi operands isEmpty ].
		newStack push: phi.
		self addInstruction: phi.
	].
	^ vmOperandStack := newStack
]

{ #category : #temporaries }
DRBasicBlock >> mergeTemporaryFramesIn: aBuilder [

	| temporaryNames newTemporaryFrame predecessorTemporaries |
	"If I have no predecessors, I'm very probably dead code.
	No need to merge anything"
	self predecessors ifEmpty: [ ^ temporaryFrame ].
	
	predecessorTemporaries := self predecessors collect: [ :e | e temporaryFrame ].
	self assert: (predecessorTemporaries collect: [ :e | e size ] as: Set) size = 1.
	
	"Do not include the return temporary variable as it is a special variable that will be resolved at the end of the method"
	temporaryNames := predecessorTemporaries flatCollect: [ :e | e keys ] as: Set.
	newTemporaryFrame := Dictionary new.
	temporaryNames collect: [ :name | | values phi |
		name ~= #'__return' ifTrue: [
			values := self predecessors collect: [ :p | p temporaryAt: name ].
			phi := (DRPhiFunction joinedVariables: values)
				result: aBuilder allocateTemporaryRegister;
				origin: thisContext stack.
			newTemporaryFrame at: name put: phi.
			self addInstruction: phi ]
		ifFalse: [ 
			newTemporaryFrame at: name put: (self predecessors anyOne temporaryAt: #'__return').
			 ] ].
	
	^ temporaryFrame := newTemporaryFrame
]

{ #category : #accessing }
DRBasicBlock >> methodNode [
	
	^ (self instructions select: [ :i | i originAST notNil ] thenCollect: [ :i | i originAST methodNode ]) anyOne
]

{ #category : #accessing }
DRBasicBlock >> numberOfPredecessors [
	
	^ predecessors size
]

{ #category : #accessing }
DRBasicBlock >> origin: aCollection [ 
	origin := aCollection
]

{ #category : #accessing }
DRBasicBlock >> phiFunctions [
	
	^ self instructions select: [ :e | e isPhiFunction ]
]

{ #category : #building }
DRBasicBlock >> phiWith: aValue [

	^ self addInstruction: (self instructionFactory phiWith: aValue)
]

{ #category : #building }
DRBasicBlock >> phiWith: aValue with: aValue2 [

	^ self addInstruction: (self instructionFactory phiWith: aValue with: aValue2)
]

{ #category : #accessing }
DRBasicBlock >> predecessors [
	^ predecessors
]

{ #category : #accessing }
DRBasicBlock >> primitiveFailureCode [
	^ primitiveFailureCode
]

{ #category : #accessing }
DRBasicBlock >> primitiveFailureCode: anInstruction [ 
	primitiveFailureCode := anInstruction
]

{ #category : #printing }
DRBasicBlock >> printOn: aStream [

	aStream nextPutAll: 'DRBasicBlock('.
	aStream print: id.
	aStream nextPutAll: ' => '.
	self successors size = 1
		ifTrue: [ aStream nextPutAll: self successors anyOne id asString ]
		ifFalse: [
			aStream nextPutAll: '{'.
			self successors
				do: [ :e | aStream nextPutAll: e id asString ]
				separatedBy: [ aStream nextPutAll: ' ,' ].
			aStream nextPutAll: '}'.
		].
	aStream nextPutAll: ')'
]

{ #category : #instructions }
DRBasicBlock >> removeInstruction: aDRInstruction [
	
	endInstruction = aDRInstruction
		ifTrue: [ ^ self endInstruction: DRNoop new ].
	
	instructions remove: aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> removePredecessor: aDRBasicBlock [ 
	
	| index |
	index := predecessors indexOf: aDRBasicBlock.
	self removePredecessorAt: index
]

{ #category : #accessing }
DRBasicBlock >> removePredecessorAt: index [ 
	
	self instructionsDo: [ :e |
		e isPhiFunction ifTrue: [ 
			e removeOperandAt: index ] ].
	self basicRemovePredecessor: (predecessors at: index)
]

{ #category : #replacing }
DRBasicBlock >> replace: anInstruction by: aReplacementInstruction [
	
	self replaceInstruction: anInstruction by: aReplacementInstruction
]

{ #category : #accessing }
DRBasicBlock >> replaceInstruction: anInstruction by: anotherInstruction [ 

	anotherInstruction basicBlock: self.

	"Check if it is in the main list of instructions"
	(instructions includes: anInstruction) ifTrue: [ 
		^ instructions at: (instructions indexOf: anInstruction) put: anotherInstruction ].

	self haltIf: [ endInstruction isKindOf: DROverflowJump ].
	"Otherwise check if it is the block end instruction"
	endInstruction ~= anInstruction
		ifTrue: [ self error: 'This block does not contain the instruction' ].
	
	endInstruction := anotherInstruction
]

{ #category : #'control-flow' }
DRBasicBlock >> replacePredecessor: aDRBasicBlock by: aDRBasicBlock2 [ 
	
	predecessors replaceAll: aDRBasicBlock with: aDRBasicBlock2
]

{ #category : #asd }
DRBasicBlock >> replaceSuccessor: aDRBasicBlock by: aDRBasicBlock2 [ 
	
	endInstruction replaceTarget: aDRBasicBlock by: aDRBasicBlock2
]

{ #category : #building }
DRBasicBlock >> setReturnValue: anInteger [
	
	^ self addInstruction: (self instructionFactory setReturnValue: anInteger)
]

{ #category : #initialization }
DRBasicBlock >> setUpInitialFailureCode [
	self shouldBeImplemented.
]

{ #category : #accessing }
DRBasicBlock >> source [
	
	^ self methodNode source
]

{ #category : #accessing }
DRBasicBlock >> sourceInterval [
	
	| nodes |
	nodes := self instructions collect: [ :i | i originAST ] thenSelect: [ :e | e notNil ].
	^ nodes first start to: nodes last stop
]

{ #category : #accessing }
DRBasicBlock >> successor [
	
	| successors |
	successors := self successors.
	successors size ~= 1 ifTrue: [ self error: 'This block does not contain a single successor' ].
	^ successors first
]

{ #category : #accessing }
DRBasicBlock >> successors [
	
	^ endInstruction targets
]

{ #category : #transformations }
DRBasicBlock >> tailDuplicate [

	"Duplicate the receiver so there is a different copy of it for each predecessor"
	
	"We can only tail duplicate if the instructions do not escape"
	"If there is an instruction in this block"
	(self instructions anySatisfy: [ :e | 
		"That has a user that is not in this block"
		e users anySatisfy: [ :user | (self instructions includes: user) not ] ])
			ifTrue: [ 
				DRError signal: 'cannot tail duplicate if values escape this block. Otherwise we will break the SSA property' ].
	
	self predecessors allButFirst do: [ :predecessorToRewire | | duplicatedBlock |
		duplicatedBlock := self basicDuplicate.
		
		self predecessors do: [ :predecessorToRemove |
			predecessorToRemove = predecessorToRewire ifFalse: [ 
				duplicatedBlock removePredecessor: predecessorToRemove ] ].
		
		self removePredecessor: predecessorToRewire.
		predecessorToRewire replaceSuccessor: self by: duplicatedBlock.
		duplicatedBlock successors do: [ :successor |
			successor addPredecessor: duplicatedBlock ].
	].
]

{ #category : #temporaries }
DRBasicBlock >> temporaryAt: aString [ 
	
	^ temporaryFrame at: aString
]

{ #category : #temporaries }
DRBasicBlock >> temporaryAt: aString put: aValue [ 
	
	self haltIf: [ (aValue isKindOf: DRValue) not ].
	temporaryFrame at: aString put: aValue
]

{ #category : #accessing }
DRBasicBlock >> temporaryFrame [
	^ temporaryFrame
]

{ #category : #accessing }
DRBasicBlock >> temporaryFrame: aTemporaryFrame [ 
	temporaryFrame := aTemporaryFrame
]

{ #category : #validating }
DRBasicBlock >> validate [

	self validatePhiFunctions.
	self validatePredecessors.
	self validateSuccessors.
	self validateInstructions
]

{ #category : #validating }
DRBasicBlock >> validateInstructions [

	self instructionsDo: [ :instruction |
		instruction validate ]
]

{ #category : #validating }
DRBasicBlock >> validatePhiFunctions [
	
	self instructionsDo: [ :e | 
		(e isPhiFunction and: [| ops |
			ops := e operands asSet.
			ops size > 1 or: [ (ops anyOne isKindOf: DRNullValue) not ] ]) ifTrue: [
			e operands size = self predecessors size
				ifFalse: [ self error: 'Phi-predecessor mismatch' ].
			
			"Validate that the order of the dependent variables is ok,
			this by checking that the variables are reachable from the correct predecessors"
			e operands withIndexDo: [ :dependency :index | 
				self assert: (dependency isCopy not
						or: [(self predecessors at: index) checkLivenessOf: dependency])
			].
		].
	]
]

{ #category : #validating }
DRBasicBlock >> validatePredecessors [
	
	self predecessors do: [ :p |
		(p successors includes: self)
			ifFalse: [ self error: 'Predecessor not pointing to me' ] ]
]

{ #category : #validating }
DRBasicBlock >> validateSuccessors [
	
	self successors do: [ :s |
		(s predecessors includes: self)
			ifFalse: [ self error: 'Successor not pointing to me' ] ]
]

{ #category : #accessing }
DRBasicBlock >> vmOperandStack [

	^ vmOperandStack
]

{ #category : #accessing }
DRBasicBlock >> vmOperandStack: anObject [

	vmOperandStack := anObject
]
