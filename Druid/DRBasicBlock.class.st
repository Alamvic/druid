Class {
	#name : #DRBasicBlock,
	#superclass : #Object,
	#instVars : [
		'id',
		'phiFunctions',
		'instructions',
		'endInstruction',
		'predecessors',
		'origin',
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

{ #category : #accessing }
DRBasicBlock >> add: anInstruction after: anotherInstruction [

	anInstruction basicBlock: self.
	(self collectionOf: anotherInstruction)
		add: anInstruction
		after: anotherInstruction
]

{ #category : #accessing }
DRBasicBlock >> add: anInstruction before: anotherInstruction [

	anInstruction basicBlock: self.
	anotherInstruction = endInstruction ifTrue: [ 
		^ instructions addLast: anInstruction ].
	(self collectionOf: anotherInstruction)
		add: anInstruction
		before: anotherInstruction
]

{ #category : #building }
DRBasicBlock >> add: aValue to: anotherValue [
	
	^ self addInstruction: (self instructionFactory add: aValue to: anotherValue)
]

{ #category : #'control-flow' }
DRBasicBlock >> addBetween: sourceBlock and: targetBlock [

	sourceBlock replaceSuccessor: targetBlock by: self.
	targetBlock replacePredecessor: sourceBlock by: self.
	self addPredecessor: sourceBlock.
	self endInstruction target: targetBlock.
	self controlFlowGraph fixBackJumps
]

{ #category : #accessing }
DRBasicBlock >> addInstruction: anInstruction [

	(self instructions includes: anInstruction) ifTrue: [ ^ self ].

	anInstruction basicBlock: self.

	^ (self collectionOf: anInstruction) add: anInstruction
]

{ #category : #accessing }
DRBasicBlock >> addInstructionFirst: anInstruction [

	(self instructions includes: anInstruction) ifTrue: [ ^ self ].

	anInstruction basicBlock: self.
		
	^ anInstruction isPhiFunction
		  ifTrue: [ phiFunctions add: anInstruction ]
		  ifFalse: [ instructions addFirst: anInstruction ]
]

{ #category : #accessing }
DRBasicBlock >> addPredecessor: aBasicBlock [

	predecessors add: aBasicBlock.
	self phiFunctions do: [ :phi | 
		phi operands size < self predecessors size ifTrue: [ phi addOperand ] ]
]

{ #category : #accessing }
DRBasicBlock >> allInstructions [
	
	^ instructions , { endInstruction  }
]

{ #category : #building }
DRBasicBlock >> and: aValue with: anotherValue [
	
	^ self addInstruction: (self instructionFactory and: aValue with: anotherValue)
]

{ #category : #building }
DRBasicBlock >> anyMask: aValue with: anotherValue [
	
	^ self addInstruction: (self instructionFactory anyMask: aValue with: anotherValue)
]

{ #category : #accessing }
DRBasicBlock >> backJumpPredecessors [

	| result |
	result := OrderedCollection new.

	self recursiveSuccessorsDo: [ :block | 
		(self predecessors includes: block) ifTrue: [ result add: block ].
		block = self ifTrue: [ ^ result ] ].

	^ result
]

{ #category : #building }
DRBasicBlock >> backJumpTo: aDRBasicBlock [ 

	^ (self jumpTo: aDRBasicBlock)
		beBackJump;
		yourself
]

{ #category : #transformations }
DRBasicBlock >> basicDuplicate [

	^ DRBlockDuplicator new
		  controlFlowGraph: controlFlowGraph;
		  basicDuplicateBlock: self
]

{ #category : #accessing }
DRBasicBlock >> basicEndInstruction: aDRInstruction [

	aDRInstruction basicBlock: self.
	^ endInstruction := aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> basicEndInstructionRemovingUsers: aDRInstruction [

	endInstruction ifNotNil: [ endInstruction operands do: [ :o | o removeUser: endInstruction ] ].
	^ self basicEndInstruction: aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> basicRemovePredecessor: aDRBasicBlock [ 
	
	predecessors remove: aDRBasicBlock
]

{ #category : #accessing }
DRBasicBlock >> beExitBlock [

	isExitBlock := true
]

{ #category : #building }
DRBasicBlock >> bitAnd: aValue with: anotherValue [
	
	^ self addInstruction: (self instructionFactory bitAnd: aValue with: anotherValue)
]

{ #category : #'control-flow' }
DRBasicBlock >> breakBy: instruction [

	"For avoid duplicate instructions on branches, this method create two predecessors chain from this basic block. Then move: 
	- instructions listed before the instruction to first (upper) predecessor.
	- the instruction to second (middle) predecessor.
	- left remaining instructions (after the instruction) in this basic block (bottom)"

	| upperPredecessor middlePredecessor instructionsBefore |
	instruction isPhiFunction ifTrue: [ 
		self error: 'Phi instructions cannot break a basic block!' ].

	upperPredecessor := self newEmptyPredecessor.
	middlePredecessor := self newEmptyPredecessor.

	instructionsBefore := self instructions allUntil: instruction.
	instructionsBefore do: [ :i | 
		self removeInstruction: i.
		upperPredecessor addInstruction: i ].


	self removeInstruction: instruction.
	middlePredecessor addInstruction: instruction.

	^ { 
		  upperPredecessor.
		  middlePredecessor.
		  self }
]

{ #category : #validating }
DRBasicBlock >> checkLivenessOf: operand [

	"Check if the value exists within this branch of execution"
	"It should be defined by myself, or by a predecessor"
	operand isGlobal ifTrue: [ ^ true ].
	^ self isDominatedBy: operand basicBlock
]

{ #category : #instructions }
DRBasicBlock >> cleanInstructions [

	"
	Remove blocks instructions from users collection in their operands.
	Warning: This method breaks the invariant user-operand.
	"

	self instructions do: [ :i | 
		i operands asSet do: [ :op | op removeUser: i ] ]
]

{ #category : #accessing }
DRBasicBlock >> collectionOf: aDRInstruction [

	^ aDRInstruction isPhiFunction
		  ifTrue: [ phiFunctions ]
		  ifFalse: [ instructions ]
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

{ #category : #accessing }
DRBasicBlock >> distanceToPredecessor: predecessorBlock [ 

	"Number of blocks between self and the argument"
	
	| forwardPredecessors |
	predecessorBlock = self ifTrue: [ ^ 0 ].
	forwardPredecessors := self forwardPredecessors.
	forwardPredecessors ifEmpty: [ ^ Float infinity ].
	^1 + (forwardPredecessors min: [ :b | b distanceToPredecessor: predecessorBlock ])

]

{ #category : #building }
DRBasicBlock >> divide: aValue by: anotherValue [
	
	^ self addInstruction: (self instructionFactory divide: aValue by: anotherValue)
]

{ #category : #'control-flow' }
DRBasicBlock >> duplicatePredecessor: existingPredecessor from: newPredecessor [
	
	"Add newPredecessor as predecessor, make sure all Phi functions in this block get a new value.
	The value should be the same as the one coming from the existing predecessor.
	
	Assumption: newPredecessor is a predecessor of existingPredecessor"
	
	self phiFunctions do: [ :e |
		e addValue: (e operandAt: (self indexOfPredecessor: existingPredecessor)) ].
	self addPredecessor: newPredecessor.

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

	self basicEndInstructionRemovingUsers: aDRInstruction.
	^ aDRInstruction
]

{ #category : #building }
DRBasicBlock >> equals: operand1 to: operand2 [ 
	
	^ self addInstruction: (self instructionFactory equals: operand1 to: operand2)
]

{ #category : #accessing }
DRBasicBlock >> first [
	
	instructions ifEmpty: [ ^ endInstruction ].
	
	^ instructions first
]

{ #category : #accessing }
DRBasicBlock >> firstInstruction [
	
	^ self instructions first
]

{ #category : #accessing }
DRBasicBlock >> forwardPredecessors [
	
	"Ignore predecessors coming from backjumps"
	^ predecessors reject: [ :e | e endInstruction isBackJumpTo: self ]
]

{ #category : #building }
DRBasicBlock >> frameReturn: aDRValue [

	^ self addInstruction: (self instructionFactory frameReturn: aDRValue)
]

{ #category : #accessing }
DRBasicBlock >> frameReturns [

	^ self instructions select: [ :i | i isFrameReturn ]
]

{ #category : #building }
DRBasicBlock >> greater: operand1 than: operand2 [
	
	^ self addInstruction: (self instructionFactory greater: operand1 than: operand2)
]

{ #category : #testing }
DRBasicBlock >> hasFinalInstruction [

	^ endInstruction notNil and: [ endInstruction isNoop not ]
]

{ #category : #dominance }
DRBasicBlock >> hasFrontier: aBlock [

	| frontier |
	frontier := controlFlowGraph tree dominanceFrontierOf: self.
	^ frontier includes: aBlock
]

{ #category : #testing }
DRBasicBlock >> hasIncomingBackjumps [
	
	^ self predecessors anySatisfy: [ :p |
		p lastInstruction isBackJumpTo: self ]
]

{ #category : #testing }
DRBasicBlock >> hasPhiFunctions [

	^ phiFunctions isNotEmpty
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
		put: self numberOfForwardPredecessors -1.

	[ workingQueue isEmpty ] whileFalse: [ 
		| currentBlock |
		currentBlock := workingQueue removeFirst.

		"Count how many times we have visited this block"
		visitsToThisBlockSoFar := visited
			                          at: currentBlock
			                          ifPresent: [ :p | p ]
			                          ifAbsent: [ 0 ].

		visited at: currentBlock put: visitsToThisBlockSoFar + 1.
		visitsToThisBlockSoFar + 1 = currentBlock numberOfForwardPredecessors
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
DRBasicBlock >> includesInstruction: anInstruction [ 
	^ instructions includes: anInstruction
]

{ #category : #accessing }
DRBasicBlock >> indexOfInstruction: anInstruction [ 
	^ self instructions indexOf: anInstruction
]

{ #category : #accessing }
DRBasicBlock >> indexOfPredecessor: aDRBasicBlock [ 
	
	^ predecessors indexOf: aDRBasicBlock
]

{ #category : #accessing }
DRBasicBlock >> initialize [
	super initialize.	
	phiFunctions := OrderedCollection new.
	instructions := OrderedCollection new.
	predecessors := OrderedCollection new.
	isExitBlock := false.

	endInstruction := DRNoop new.
	endInstruction basicBlock: self.
]

{ #category : #splitting }
DRBasicBlock >> insertPhisAndReplaceUsersBetween: myInstruction andAll: duplicatedInstructions withFrontier: frontier withUsers: users [

	| nextFrontier valuesToMerge phis |
	self flag: #TODO. "Move this responsability to another object"

	users ifEmpty: [ "Finish" ^ self ].
	myInstruction hasNonLocalUsers ifFalse: [ "Nothing to do"
		self assert: users isEmpty.
		^ self ].
	frontier isEmpty ifTrue: [ "Last useful block"
		self assert: users isEmpty.
		^ self ].

	valuesToMerge := { myInstruction } , duplicatedInstructions.

	"Create phis on frontier"
	phis := frontier collect: [ :b | b phiWithVariables: valuesToMerge ].

	"Replace dominated users"
	users copy do: [ :u | 
		(u replaceOperandsIfDominated: myInstruction withAll: phis) 
			ifNotNil: [ users remove: u ] ].

	"Create the phi of phis in the frontier and update uses based on blocks dominance"
	nextFrontier := controlFlowGraph tree minimalDominanceFrontierOfAll:
		                frontier.
	^ self
		  insertPhisAndReplaceUsersBetween: myInstruction
		  andAll: phis
		  withFrontier: nextFrontier
		  withUsers: users
]

{ #category : #inspection }
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

{ #category : #'enumerating instructions' }
DRBasicBlock >> instructionAfter: anInstruction [
	
	"Phi functions are always at the beginning of the block and they are conceptually executed all at the same time
	Thus, their next instruction is the first instruction in the instruction list"
	anInstruction isPhiFunction ifTrue: [ 
		^ self first ].

	"If the next instruction is the last in the list, return the block end instruction"
	instructions last = anInstruction
		ifTrue: [ ^ endInstruction ].
	
	"Otherwise, the next instruction is the next in the list"
	^ instructions after: anInstruction
]

{ #category : #'enumerating instructions' }
DRBasicBlock >> instructionBefore: anInstruction [

	anInstruction isPhiFunction ifTrue: [ 
		"Phi functions are always at the beginning of the block and they are conceptually executed all at the same time
		However, we may want to consider ordering when navigating across phis
		For now, this is intentionally left unimplemented"
		^ self notYetImplemented ].

	endInstruction = anInstruction
		ifTrue: [ ^ self instructions last ].

	instructions first = anInstruction
		"Not defined yet. Could return either a phi, or nothing"
		ifTrue: [ ^ nil ].
	
	"Otherwise, the previous instruction is the previous in the list"
	^ instructions before: anInstruction
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

	phiFunctions copy do: aFullBlockClosure.
	instructions copy do: aFullBlockClosure.
	endInstruction ifNotNil: [ aFullBlockClosure value: endInstruction ]
]

{ #category : #building }
DRBasicBlock >> intToFloat: aValue [
	
	^ self addInstruction: (self instructionFactory intToFloat: aValue)
]

{ #category : #'testing - dominator' }
DRBasicBlock >> isDominatedBy: dominatorBlock [
	
	^ controlFlowGraph tree isDominator: dominatorBlock of: self
]

{ #category : #building }
DRBasicBlock >> isDominatorOf: basicBlocks [
	"Deprecated: should be removed once it's posible. Use DRBasicBlock>#isDominatedBy: instead."
	| dominatorTree node |
	
	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: controlFlowGraph.
	node := dominatorTree blockDictionary at: self.
	node children size = basicBlocks size ifTrue: [
		node children do: [ :child |
			basicBlocks indexOf: child block ifAbsent: [ ^ false ].
		].
		^ true
	].
	
	^ false
]

{ #category : #testing }
DRBasicBlock >> isEmptyBasicBlock [

	^ endInstruction isNoop and: [ self instructions allButLast isEmpty ]
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
DRBasicBlock >> jitCompileTimeVariable: aName [

	^ self instructionFactory jitCompileTimeVariable: aName
]

{ #category : #building }
DRBasicBlock >> jumpIf: aCondition [
	
	^ self endInstruction: (self instructionFactory jumpIf: aCondition)
]

{ #category : #building }
DRBasicBlock >> jumpIf: condition to: aDRBasicBlock ifFalseTo: aDRBasicBlock2 [
	" Used for removing an existing edge that jumps to the target block,
	 for being able to relink to the target block with a new desired jump "
	aDRBasicBlock addPredecessor: self.
	aDRBasicBlock2 addPredecessor: self.

	^ self endInstruction: (DRBranchIfTrue new
			   operands: { condition };
			   trueBranch: aDRBasicBlock;
			   falseBranch: aDRBasicBlock2;
			   yourself)
]

{ #category : #building }
DRBasicBlock >> jumpIfTrueTo: aDRBasicBlock ifFalseTo: aDRBasicBlock2 [

	^ self
		  jumpIf: true asDRValue
		  to: aDRBasicBlock
		  ifFalseTo: aDRBasicBlock2
]

{ #category : #building }
DRBasicBlock >> jumpTo: aBasicBlock [

	self endInstruction: (DRJump new target: aBasicBlock).
	(aBasicBlock hasPredecessor: self) ifFalse: [ 
		aBasicBlock addPredecessor: self ].
	^ endInstruction
]

{ #category : #accessing }
DRBasicBlock >> lastInstruction [
	
	^ endInstruction
]

{ #category : #building }
DRBasicBlock >> load: anAddress [

	^ self addInstruction: (self instructionFactory load: anAddress)
]

{ #category : #building }
DRBasicBlock >> loadArgument: index [

	^ self addInstruction: (self instructionFactory loadArgument: index)
]

{ #category : #building }
DRBasicBlock >> loadFramePointer [

	^ self addInstruction: self instructionFactory loadFramePointer
]

{ #category : #building }
DRBasicBlock >> loadSInt64: operand [
	
	^ self addInstruction: (self instructionFactory loadSInt64: operand)
]

{ #category : #building }
DRBasicBlock >> loopIf: loopCondition to: loopBodyBlock ifFalseTo: loopExitBlock [

	self jumpIf: loopCondition to: loopBodyBlock ifFalseTo: loopExitBlock.
	loopBodyBlock backJumpTo: self
]

{ #category : #splitting }
DRBasicBlock >> mergeInstructionsWithDuplicatedBlock: duplicatedBlock withSuccessors: successors usingUsers: getUsersBlock [

	self instructions copy
		with: duplicatedBlock instructions copy
		do: [ :myInstruction :duplicatedInstruction | 
			self
				insertPhisAndReplaceUsersBetween: myInstruction
				andAll: { duplicatedInstruction }
				withFrontier: successors
				withUsers: (getUsersBlock value: myInstruction) ]
]

{ #category : #building }
DRBasicBlock >> messageSend: selector withOperands: operands [

	^ self addInstruction: (self instructionFactory messageSend: selector withOperands: operands)
]

{ #category : #accessing }
DRBasicBlock >> methodNode [
	
	^ (self instructions select: [ :i | i originAST notNil ] thenCollect: [ :i | i originAST methodNode ]) anyOne
]

{ #category : #building }
DRBasicBlock >> modulo: aValue by: anotherValue [
	
	^ self addInstruction: (self instructionFactory modulo: aValue by: anotherValue)
]

{ #category : #building }
DRBasicBlock >> multiply: aValue by: anotherValue [
	
	^ self addInstruction: (self instructionFactory multiply: aValue by: anotherValue)
]

{ #category : #'control-flow' }
DRBasicBlock >> newEmptyPredecessor [

	| newBlock |
	newBlock := controlFlowGraph newBasicBlock.
	self phiFunctions do: [ :i | newBlock addInstruction: i ].
	phiFunctions := OrderedCollection new.
	predecessors copy do: [ :b | 
		newBlock addPredecessor: b.
		b replaceSuccessor: self by: newBlock.
		self removePredecessor: b ].
	self addPredecessor: newBlock.
	newBlock jumpTo: self.
	^ newBlock
]

{ #category : #building }
DRBasicBlock >> newEmptySuccessors [
	"Create new block for each successor and insert it in between"

	^ self successors collect: [ :b | 
		| newBlock |
		newBlock := controlFlowGraph newBasicBlock.
		newBlock addPredecessor: self.
		self replaceSuccessor: b by: newBlock.
		b replacePredecessor: self by: newBlock.
		newBlock jumpTo: b.
		newBlock ].
]

{ #category : #building }
DRBasicBlock >> newPhi [

	"Explain what it does"
	^ self phiWithVariables: #(  )
]

{ #category : #accessing }
DRBasicBlock >> numberOfForwardPredecessors [
	
	"Ignore predecessors coming from backjumps"
	^ predecessors count: [ :e | (e endInstruction isBackJumpTo: self) not ]
]

{ #category : #building }
DRBasicBlock >> or: aValue with: anotherValue [
	
	^ self addInstruction: (self instructionFactory or: aValue with: anotherValue)
]

{ #category : #accessing }
DRBasicBlock >> origin: aCollection [ 
	origin := aCollection
]

{ #category : #accessing }
DRBasicBlock >> phiFunctions [
	
	^ phiFunctions
]

{ #category : #building }
DRBasicBlock >> phiWith: aValue [

	^ self phiWithVariables: { aValue }
]

{ #category : #building }
DRBasicBlock >> phiWith: aValue with: aValue2 [

	^ self phiWithVariables: { aValue. aValue2 }
]

{ #category : #building }
DRBasicBlock >> phiWithVariables: variables [

	| finalVariables allPosibleVariables phi |
	controlFlowGraph buildDominatorTree.

	allPosibleVariables := variables asOrderedCollection.
	self predecessors size > variables size ifTrue: [ 
		self predecessors size - variables size timesRepeat: [ 
			allPosibleVariables := allPosibleVariables , variables ] ].

	finalVariables := self predecessors collect: [ :b | 
		                  allPosibleVariables
			                  detect: [ :i | b checkLivenessOf: i ]
			                  ifFound: [ :i | allPosibleVariables remove: i ]
			                  ifNone: [ DRNullValue new ] ].

	phi := self instructionFactory phiWithVariables: finalVariables.
	^ self addInstruction: phi
]

{ #category : #accessing }
DRBasicBlock >> predecessor [
	
	| predecessors |
	predecessors := self predecessors.
	predecessors size ~= 1 ifTrue: [ self error: 'This block does not contain a single predecessor' ].
	^ predecessors first
]

{ #category : #accessing }
DRBasicBlock >> predecessorAt: index [
	
	^ predecessors at: index
]

{ #category : #accessing }
DRBasicBlock >> predecessors [
	^ predecessors
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

{ #category : #building }
DRBasicBlock >> push: operand [
	
	^ self addInstruction: (self instructionFactory push: operand)
]

{ #category : #building }
DRBasicBlock >> pushBase: base offset: offset [
	
	^ self addInstruction: (self instructionFactory pushBase: base offset: offset)
]

{ #category : #iterating }
DRBasicBlock >> recursivePredecessors [

	| recursivePredecessors |
	recursivePredecessors := OrderedCollection new.
	self recursivePredecessorsDo: [ :b | recursivePredecessors add: b ].
	^ recursivePredecessors
]

{ #category : #iterating }
DRBasicBlock >> recursivePredecessorsDo: aBlock [

	self recursivePredecessorsDo: aBlock visited: {  }
]

{ #category : #iterating }
DRBasicBlock >> recursivePredecessorsDo: aBlock visited: alreadyVisited [

	(alreadyVisited includes: self) ifTrue: [ ^ self ].
	self predecessors do: [ :b | aBlock value: b ].
	self predecessors do: [ :b | 
		b recursivePredecessorsDo: aBlock visited: { self } , alreadyVisited ]
]

{ #category : #iterating }
DRBasicBlock >> recursiveSuccessors [

	| recursiveSuccessors |
	recursiveSuccessors := OrderedCollection new.
	self recursiveSuccessorsDo: [ :b | recursiveSuccessors add: b ].
	^ recursiveSuccessors
]

{ #category : #iterating }
DRBasicBlock >> recursiveSuccessorsDo: aBlock [

	self recursiveSuccessorsDo: aBlock visited: {  }
]

{ #category : #iterating }
DRBasicBlock >> recursiveSuccessorsDo: aBlock visited: alreadyVisited [

	(alreadyVisited includes: self) ifTrue: [ ^ self ].
	self successors do: [ :b | aBlock value: b ].
	self successors do: [ :b | 
		b recursiveSuccessorsDo: aBlock visited: { self } , alreadyVisited ]
]

{ #category : #'control-flow' }
DRBasicBlock >> removeFromCFG [

	self cleanInstructions.
	controlFlowGraph removeBasicBlock: self
]

{ #category : #'control-flow' }
DRBasicBlock >> removeFromCFGAndDisconnectSuccessors [

	self successors copy do: [ :succ | succ removePredecessor: self ].
	self removeFromCFG
]

{ #category : #instructions }
DRBasicBlock >> removeInstruction: aDRInstruction [

	endInstruction = aDRInstruction ifTrue: [ 
		^ self endInstruction: DRNoop new ].

	aDRInstruction isPhiFunction
		ifTrue: [ phiFunctions remove: aDRInstruction ]
		ifFalse: [ instructions remove: aDRInstruction ]
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

{ #category : #instructions }
DRBasicBlock >> replace: anInstruction by: aReplacementInstruction [
	
	self replaceInstruction: anInstruction by: aReplacementInstruction
]

{ #category : #accessing }
DRBasicBlock >> replaceInstruction: anInstruction by: anotherInstruction [

	anotherInstruction basicBlock = self ifTrue: [ 
		^ self removeInstruction: anInstruction ].

	anotherInstruction basicBlock: self.

	"Check if it is in phi functions"
	(phiFunctions includes: anInstruction) ifTrue: [ 
		^ anotherInstruction isPhiFunction
			  ifTrue: [ 
				  phiFunctions
					  at: (phiFunctions indexOf: anInstruction)
					  put: anotherInstruction ]
			  ifFalse: [ 
				  phiFunctions remove: anInstruction.
				  instructions addFirst: anotherInstruction ] ].

	"Check if it is in the main list of instructions"
	(instructions includes: anInstruction) ifTrue: [ 
		^ instructions
			  at: (instructions indexOf: anInstruction)
			  put: anotherInstruction ].

	"Otherwise check if it is the block end instruction"
	endInstruction ~= anInstruction ifTrue: [ 
		self error: 'This block does not contain the instruction' ].

	endInstruction := anotherInstruction
]

{ #category : #'control-flow' }
DRBasicBlock >> replacePredecessor: aDRBasicBlock by: aDRBasicBlock2 [ 
	
	predecessors replaceAll: aDRBasicBlock with: aDRBasicBlock2
]

{ #category : #'control-flow' }
DRBasicBlock >> replaceSuccessor: aDRBasicBlock by: aDRBasicBlock2 [ 
	
	endInstruction replaceTarget: aDRBasicBlock by: aDRBasicBlock2
]

{ #category : #building }
DRBasicBlock >> return [

	^ self endInstruction: DRReturn new
]

{ #category : #building }
DRBasicBlock >> return: anInstruction [ 
	^ self endInstruction: (DRReturn operands: { anInstruction } )
]

{ #category : #accessing }
DRBasicBlock >> second [
	
	^ self allInstructions second
]

{ #category : #building }
DRBasicBlock >> setReturnValue: anInteger [
	
	^ self addInstruction: (self instructionFactory setReturnValue: anInteger)
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

{ #category : #splitting }
DRBasicBlock >> splitTo: predecessorToRewire [

	| duplicatedBlock newSuccessors |
	self assert: self predecessors size > 1.

	newSuccessors := self newEmptySuccessors.

	duplicatedBlock := self basicDuplicate.
	duplicatedBlock successors do: [ :successor | 
		successor addPredecessor: duplicatedBlock ].

	self predecessors do: [ :predecessorToRemove | 
		predecessorToRemove = predecessorToRewire ifFalse: [ 
			duplicatedBlock removePredecessor: predecessorToRemove ] ].

	self removePredecessor: predecessorToRewire.
	predecessorToRewire replaceSuccessor: self by: duplicatedBlock.

	controlFlowGraph tree:
		(DRDominatorTree new buildDominatorTreeOf: controlFlowGraph).
	controlFlowGraph tree computeDominanceFrontier.

	self
		mergeInstructionsWithDuplicatedBlock: duplicatedBlock
		withSuccessors: newSuccessors
		usingUsers: [:i | i nonLocalUsers].

	^ duplicatedBlock
]

{ #category : #building }
DRBasicBlock >> store: instruction in: memoryAddress [

	^ self addInstruction:
		  (self instructionFactory store: instruction in: memoryAddress)
]

{ #category : #building }
DRBasicBlock >> store: aValue into: anAddress [

	^ self addInstruction: (self instructionFactory store: aValue into: anAddress)
]

{ #category : #building }
DRBasicBlock >> storeSInt64: operand at: address [

	^ self addInstruction: (self instructionFactory storeSInt64: operand at: address)
]

{ #category : #building }
DRBasicBlock >> strongCopy: aValue [
	
	^ self addInstruction: (self instructionFactory strongCopy: aValue)
]

{ #category : #building }
DRBasicBlock >> subtract: aValue from: anotherValue [
	
	^ self addInstruction: (self instructionFactory subtract: aValue from: anotherValue)
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

	| duplicatedBlocks |
	duplicatedBlocks := self predecessors allButFirst collect: [ 
		                    :predecessorToRewire | 
		                    self splitTo: predecessorToRewire ].

	self assert: (duplicatedBlocks allSatisfy: [ :b | 
			 b instructions size = self instructions size ]).

	controlFlowGraph validate.

	^ { self } , duplicatedBlocks
]

{ #category : #accessing }
DRBasicBlock >> third [
	
	^ self allInstructions third
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

	(self instructions allSatisfy: [ :i | i basicBlock = self ]) 
		ifFalse: [ DRError signal: 'Block-instruction mismatch in block: ' , self asString ].

	self instructionsDo: [ :instruction | instruction validate ]
]

{ #category : #validating }
DRBasicBlock >> validatePhiFunctions [

	self phiFunctions do: [ :e | 
		| ops |
		ops := e operands asIdentitySet.
		(ops size > 1 and: [ (ops noneSatisfy: [ :o | o isKindOf: DRNullValue])  ]) 
			ifTrue: [ 
				e operands size = self predecessors size ifFalse: [ 
					self error: 'Phi-predecessor mismatch' ].

				"Validate that the order of the dependent variables is ok,
				this by checking that the variables are reachable from the correct predecessors"
				e operands withIndexDo: [ :dependency :index | 
					self assert: (dependency isInstruction or: [ 
							 dependency isNullValue or: [ dependency isPhysicalRegister or: [ dependency isDRBlockClosure or: [ dependency value class = DruidTestInterpreter  ] ] ] ]).
					self assert: (dependency isInstruction not or: [ 
							 (self predecessors at: index) checkLivenessOf: dependency ]) ] ] ]
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

{ #category : #copying }
DRBasicBlock >> veryDeepCopyWith: deepCopier [
	deepCopier references at: origin ifAbsentPut: [nil].
	^ super veryDeepCopyWith: deepCopier
]
