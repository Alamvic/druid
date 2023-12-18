Class {
	#name : #DRInstruction,
	#superclass : #DRValue,
	#instVars : [
		'operands',
		'result',
		'order',
		'origin',
		'originAST',
		'basicBlock',
		'users',
		'classType'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance creation' }
DRInstruction class >> operands: aCollection [
	
	"Validate operands collections size?"
	^ self new
		operands: aCollection;
		yourself
]

{ #category : #'instance creation' }
DRInstruction class >> operands: aCollection result: aResultRegister [
	
	"Validate operands collections size?"
	^ self new
		operands: aCollection;
		result: aResultRegister;
		yourself
]

{ #category : #'instance creation' }
DRInstruction class >> result: aResultRegister [

	^ self operands: #() result: aResultRegister
]

{ #category : #testing }
DRInstruction >> = anObject [ 

	self class = anObject class ifTrue: [ 
		self result = anObject result ifTrue: [
			self operands = anObject operands ifTrue: [
				^ true. ]]].
	^ false. 
]

{ #category : #visiting }
DRInstruction >> acceptVisitor: aVisitor withBranch: aDRPhiFunction [ 
	
	^ self acceptVisitor: aVisitor
]

{ #category : #accessing }
DRInstruction >> addBefore: anotherInstruction [
	
	basicBlock add: anotherInstruction before: self
]

{ #category : #accessing }
DRInstruction >> addNext: anotherInstruction [
	
	basicBlock add: anotherInstruction after: self
]

{ #category : #accessing }
DRInstruction >> addOperand: anOperand [

	operands add: anOperand.
	anOperand addUser: self.
]

{ #category : #'users-definitions' }
DRInstruction >> addUser: anInstruction [

	users add: anInstruction
]

{ #category : #accessing }
DRInstruction >> asNumberedExpression [

	| numberedExpression |
	
	numberedExpression := DRNumberedExpression new.
	^ numberedExpression operation: self class;
	operands: self operands;
	yourself
]

{ #category : #accessing }
DRInstruction >> basicBlock [

	^ basicBlock
]

{ #category : #accessing }
DRInstruction >> basicBlock: anObject [

	basicBlock := anObject
]

{ #category : #'cfg manipulation' }
DRInstruction >> breakBasicBlock [

	^ self basicBlock breakBy: self
]

{ #category : #'loop-invariance' }
DRInstruction >> canMoveOutsideLoop: loop [

	^ (self operands allSatisfy: [ :operand | operand isInvariantFrom: loop ]) and: [ 
		"Can not be inside a if (inside the loop). 
		In that case also depends of conditional jump operands"
		  (self controlFlowGraph allBlocksBetween: loop headerBlock and: self basicBlock)
			  noneSatisfy: [ :middleBlock | middleBlock endInstruction isConditionalBranch ] ]
]

{ #category : #'users-definitions' }
DRInstruction >> clearUsers [

	users removeAll
]

{ #category : #accessing }
DRInstruction >> controlFlowGraph [
	
	^ basicBlock controlFlowGraph
]

{ #category : #copying }
DRInstruction >> copyToVariableIn: aDRPrimitiveIRGenerator inNode: aRBReturnNode [ 
	
	"I'm an already an instruction with a value. No need to do anything"
	^ self
]

{ #category : #testing }
DRInstruction >> hasDependency: anInstruction [
	
	^ self dependencies includes: anInstruction
]

{ #category : #testing }
DRInstruction >> hasNonLocalUsers [

	"Answer if exist any user outside the block"

	^ users anySatisfy: [ :u | u basicBlock ~= basicBlock ]
]

{ #category : #testing }
DRInstruction >> hasOperand: anInstruction [
	
	^ operands includes: anInstruction
]

{ #category : #testing }
DRInstruction >> hasSameExpressionAs: instruction [

	self class = instruction class ifTrue: [ 
		self operands = instruction operands ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #testing }
DRInstruction >> hasUser: anInstruction [
	
	^ users includes: anInstruction
]

{ #category : #testing }
DRInstruction >> hasUsers [
	
	^ users notEmpty
]

{ #category : #testing }
DRInstruction >> hash [

	^ self result hash
]

{ #category : #accessing }
DRInstruction >> indexInBasicBlock [
	^ self basicBlock indexOfInstruction: self
]

{ #category : #testing }
DRInstruction >> initialize [

	super initialize.
	operands := #().
	users := IdentitySet new.
]

{ #category : #inspecting }
DRInstruction >> inspectionDependencies: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Dependency Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllDependencies asSet collect: [ :c | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: c label asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: Color white;
			border: (RSBorder new color: Color purple; width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: c;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #dependencies.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : #inspecting }
DRInstruction >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>
	
	^ originAST inspectionSourceCode
]

{ #category : #inspecting }
DRInstruction >> inspectionUsers: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Users Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllUsers asSet collect: [ :c | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: c label asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: Color white;
			border: (RSBorder new color: Color purple; width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: c;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #users.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : #accessing }
DRInstruction >> instructionFactory [
	
	^ basicBlock instructionFactory
]

{ #category : #accessing }
DRInstruction >> isBackJump [

	^ false
]

{ #category : #testing }
DRInstruction >> isCogitSendMarshall [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isCommutative [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isConditionalBranch [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isCopy [
	^ false
]

{ #category : #testing }
DRInstruction >> isDivision [
	
	^ false
]

{ #category : #dominance }
DRInstruction >> isDominatedBy: anotherOperand [

	^ anotherOperand basicBlock = self basicBlock or: [ 
		  self basicBlock isDominatedBy: anotherOperand basicBlock ]
]

{ #category : #accessing }
DRInstruction >> isInstruction [

	^ true
]

{ #category : #'loop-invariance' }
DRInstruction >> isInvariantFrom: loop [

	^ loop headerBlock isDominatedBy: self basicBlock
]

{ #category : #testing }
DRInstruction >> isJITCompileTimeExpression: aString [ 
	
	^ false
]

{ #category : #testing }
DRInstruction >> isJITMessageSend [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isLoadInstanceVariable [

	^ false
]

{ #category : #testing }
DRInstruction >> isLoadTemporaryVariable [

	^ false
]

{ #category : #testing }
DRInstruction >> isModulo [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isMultiply [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isNonConditionalJump [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isPhiFunction [

	^ false
]

{ #category : #testing }
DRInstruction >> isPop [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isValue [

	^ false
]

{ #category : #accessing }
DRInstruction >> label [

	^ self opcode, '(', (self result ifNotNil: [ :e | e name ] ifNil: [self]) asString, ')'
]

{ #category : #interpretation }
DRInstruction >> lookupClass [
	
	^ self type lookupClass 
]

{ #category : #moving }
DRInstruction >> moveToDominatingBlock: destinationBlock [

	self basicBlock removeInstruction: self.
	destinationBlock addInstruction: self.
	self rewirePhiOperandsTo: destinationBlock
]

{ #category : #accessing }
DRInstruction >> name [
	
	^ result name
]

{ #category : #testing }
DRInstruction >> needsStackFlush [
	
	^ false
]

{ #category : #accessing }
DRInstruction >> nextInstruction [
	
	^ self basicBlock instructionAfter: self
]

{ #category : #accessing }
DRInstruction >> nonLocalUsers [

	^ self users reject: [ :i | i basicBlock = self basicBlock ]
]

{ #category : #printing }
DRInstruction >> opcode [
	
	^ self class name
]

{ #category : #accessing }
DRInstruction >> operand1 [
	
	^ operands at: 1
]

{ #category : #accessing }
DRInstruction >> operand1: aDRAbsoluteMemoryAddress [ 
	self shouldBeImplemented.
]

{ #category : #accessing }
DRInstruction >> operand2 [

	^ operands at: 2 ifAbsent: [nil]
]

{ #category : #accessing }
DRInstruction >> operand3 [

	^ operands at: 3 ifAbsent: [nil]
]

{ #category : #operands }
DRInstruction >> operandAt: anInteger [
	
	^ operands at: anInteger
]

{ #category : #operands }
DRInstruction >> operandAt: anInteger put: aValue [ 
	
	operands at: anInteger put: aValue
]

{ #category : #operands }
DRInstruction >> operandAtPredecessor: aPredecessor [
	
	^ operands at: (self basicBlock indexOfPredecessor: aPredecessor)
]

{ #category : #accessing }
DRInstruction >> operands [
	^ operands
]

{ #category : #accessing }
DRInstruction >> operands: aCollection [ 

	operands asSet do: [ :each | each removeUser: self ].
	operands := aCollection asOrderedCollection.
	self dependencies asSet do: [ :each | each addUser: self ]
]

{ #category : #operands }
DRInstruction >> operandsAtPredecessorsOtherThan: aPredecessor [
	
	| index |
	index := self basicBlock indexOfPredecessor: aPredecessor.
	^ operands withIndexSelect: [ :op :in | in ~= index ]
]

{ #category : #accessing }
DRInstruction >> order [
	
	^ order
]

{ #category : #accessing }
DRInstruction >> order: anInteger [ 
	order := anInteger
]

{ #category : #accessing }
DRInstruction >> origin [

	^ origin
]

{ #category : #accessing }
DRInstruction >> origin: anObject [

	origin := anObject
]

{ #category : #accessing }
DRInstruction >> originAST [
	
	^ originAST
]

{ #category : #accessing }
DRInstruction >> originAST: aNode [
	
	originAST := aNode
]

{ #category : #copying }
DRInstruction >> postCopy [

	super postCopy.
	
	operands := operands copy.
	operands do: [ :e | e addUser: self ].
	users := users copy.
	
	((result isKindOf: DRNoRegister) not and: [ result isPhysicalRegister not ]) ifTrue: [ 
		result := basicBlock controlFlowGraph allocateTemporaryRegister ].

]

{ #category : #printing }
DRInstruction >> prettyPrint [

	| print |
	print := result ifNil: [ '' ] ifNotNil: [ result name , ' := ' ].
	
	^ print , self opcode , ' '
	  , (' ' join: (self operands collect: [ :e | e ifNotNil: [ e printResult ] ]))
]

{ #category : #accessing }
DRInstruction >> previousInstruction [
	
	^ self basicBlock instructionBefore: self
]

{ #category : #printing }
DRInstruction >> printOn: aStream [

	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self prettyPrint.
	aStream nextPut: $)
]

{ #category : #printing }
DRInstruction >> printResult [

	result ifNotNil: [ ^ result name ].
	^ self opcode
]

{ #category : #'users-definitions' }
DRInstruction >> recursiveUsersDo: aBlock [

	self recursiveUsersDo: aBlock iteratedSet: Set new
]

{ #category : #'users-definitions' }
DRInstruction >> recursiveUsersDo: aBlock iteratedSet: set [

	"If I'm already done, exit"
	(set includes: self)
		ifTrue: [ ^ self ].
	set add: self.
	
	users do: [ :i | 
		aBlock value: i.
		i recursiveUsersDo: aBlock iteratedSet: set ]
]

{ #category : #removing }
DRInstruction >> removeFromCFG [

	basicBlock removeInstruction: self.
	self operands asSet do: [ :e | e removeUser: self ].
	self users asSet do: [ :e | e replaceDependency: self by: DRNullValue new  ]
]

{ #category : #operands }
DRInstruction >> removeOperandAt: anInteger [ 

	| removed |
	operands := operands asOrderedCollection.
	removed := operands removeAt: anInteger.
	
	"It could happen a same instruction has the same operand twice.
	Then only remove the user relationship if its not in the operands list after removal"
	(operands includes: removed)
		ifFalse: [ removed removeUser: self ]
]

{ #category : #'users-definitions' }
DRInstruction >> removeUser: anInstruction [

	users remove: anInstruction
]

{ #category : #replacing }
DRInstruction >> replaceBy: anotherInstruction [
	
	self = anotherInstruction ifTrue: [ ^ self ].
	
	basicBlock replace: self by: anotherInstruction.
	
	self replaceUsesBy: anotherInstruction.

	operands asSet do: [ :instruction |
		instruction removeUser: self ].
]

{ #category : #'users-definitions' }
DRInstruction >> replaceDependency: anOperand by: anotherOperand [

	self replaceOperand: anOperand by: anotherOperand
]

{ #category : #dependencies }
DRInstruction >> replaceDependency: anOperand by: anotherOperand in: anInstruction [
	
	anInstruction replaceOperand: anOperand by: anotherOperand
]

{ #category : #replacing }
DRInstruction >> replaceDominatedUsesBy: anotherInstruction [

	users
		select: [ :instruction | 
			instruction isDominatedBy: anotherInstruction ]
		thenDo: [ :instruction | 
			instruction replaceDependency: self by: anotherInstruction ]
]

{ #category : #'users-definitions' }
DRInstruction >> replaceOperand: anOperand by: anotherOperand [

	self flag: #careful.
	"If we have the same operand twice, this will BREAK"
	
	anOperand removeUser: self.
	anotherOperand addUser: self.

	operands replaceAll: anOperand with: anotherOperand
]

{ #category : #'users-definitions' }
DRInstruction >> replaceOperandAtIndex: index by: anotherOperand [

	| operand |
	operand := operands at: index.

	operands at: index put: anotherOperand.

	anotherOperand addUser: self.
	"Only remove myself as user if we just removed the LAST use"
	(operands includes: operand)
		ifFalse: 	[operand removeUser: self].
]

{ #category : #operands }
DRInstruction >> replaceOperandIfDominated: myOperand by: anotherOperand [

	(self isDominatedBy: anotherOperand) ifFalse: [ ^ nil ].
	self replaceDependency: myOperand by: anotherOperand
]

{ #category : #replace }
DRInstruction >> replaceOperandsIfDominated: myOperand withAll: newOperands [

	| replaced |
	replaced := nil.
	newOperands do: [ :newOperand | 
		(self replaceOperandIfDominated: myOperand by: newOperand) 
			ifNotNil: [ replaced := self ] ].
	^ replaced
]

{ #category : #replacing }
DRInstruction >> replaceUsesBy: anotherInstruction [
	
	users copy do: [ :instruction |
		instruction replaceDependency: self by: anotherInstruction ]
]

{ #category : #accessing }
DRInstruction >> result [

	^ result
]

{ #category : #accessing }
DRInstruction >> result: anObject [

	result := anObject
]

{ #category : #operands }
DRInstruction >> reverseOperands [
	
	operands := operands reversed
]

{ #category : #moving }
DRInstruction >> rewirePhiOperandsTo: destinationBlock [

	self operands
		select: [ :operand | operand isPhiFunction and: [(self basicBlock isDominatedBy: operand basicBlock) not]  ]
		thenDo: [ :operand | 
			| newOperandValues |
			"Note: this only works when moving to parent block"
			newOperandValues := operand valuesForPredecessor: destinationBlock.
			self assert: [ newOperandValues size = 1 ].
			self replaceOperand: operand by: newOperandValues anyOne ]
]

{ #category : #cogit }
DRInstruction >> rtlPushArgumentExpressions: generator [
	
	^ { RBVariableNode named: (generator allocateVariable: result) }
]

{ #category : #cogit }
DRInstruction >> rtlPushOperandQualifierForInstruction: anInstruction [
	
	^ 'Register:'
]

{ #category : #SCCP }
DRInstruction >> sccpEvaluateFor: sccp [

	| previousValue newValue |
	previousValue := sccp latticeValue: self.
	previousValue = sccp bottom ifTrue: [ ^ self ].

	newValue := self sccpLatticeValueFor: sccp.

	previousValue = newValue ifFalse: [
		"Set thew new value and invalidate the uses"
		sccp setValueOf: self at: newValue ]
]

{ #category : #SCCP }
DRInstruction >> sccpLatticeValueFor: sccp [

	| operand1 operand2 |
	operand1 := sccp latticeValue: self operand1.
	operand2 := sccp latticeValue: self operand2.

	^ sccp
		  sccpLatticeValueFor: self
		  latticeOperand1: operand1
		  latticeOperand2: operand2
]

{ #category : #SCCP }
DRInstruction >> sccpOperateOnLattice: operand and: operand2 [
	
	self subclassResponsibility
]

{ #category : #testing }
DRInstruction >> supportConstantOperand [

	^ true
]

{ #category : #types }
DRInstruction >> type [

	^ DRSignedIntegerType size: 8 "Word size"
]

{ #category : #inspecting }
DRInstruction >> uiUnroll [

	| unrollingFactor |
	self isBackJump ifFalse: [ 
		^ self inform: 'Can only perform loop unrolling from back jumps' ].
	unrollingFactor := UIManager default
		                   request: 'Unrolling factor:'
		                   initialAnswer: '2'.
	(unrollingFactor ifNil: [ '' ]) ifEmpty: [ unrollingFactor := 2 ].
	self loop unroll: unrollingFactor asNumber
]

{ #category : #replacing }
DRInstruction >> users [

	^ users
]

{ #category : #validation }
DRInstruction >> validate [

	self validateDependenciesAreUsedByMyself.
	self validateUsersUseMyself.

	self isPhiFunction ifTrue: [ ^ self ].
	self operands do: [ :o | 
		(o isInstruction not or: [ 
			 self basicBlock isDominatedBy: o basicBlock ]) ifFalse: [ 
			DRError signal: 'Invalid operand: ', o asString, ' in: ' , self asString ] ]
]

{ #category : #validation }
DRInstruction >> validateDependenciesAreUsedByMyself [

	self dependencies do: [ :op | 
		(op hasUser: self) ifFalse: [ 
			DRError signal:
				'Dependency-user mismatch between: ' , self asString , ' and: '
				, op asString ] ]
]

{ #category : #validation }
DRInstruction >> validateUsersUseMyself [

	self users do: [ :user | 
		(user hasDependency: self) ifFalse: [ 
			DRError signal:
				'User-operand mismatch between: ' , user asString , ' and: '
				, self asString ] ]
]

{ #category : #copying }
DRInstruction >> veryDeepCopyWith: deepCopier [
	deepCopier references at: origin ifAbsentPut: [nil].
	^ super veryDeepCopyWith: deepCopier
]
