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
		'users'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRInstruction class >> operands: operands [

	^ self operands: operands result: DRNoRegister new
]

{ #category : #'instance creation' }
DRInstruction class >> operands: aCollection result: aResultRegister [
	
	"Validate operands collections size?"
	^ self new
		operands: aCollection;
		result: aResultRegister;
		yourself
]

{ #category : #testing }
DRInstruction >> = anObject [ 

	self class = anObject class ifTrue: [ 
		self result = anObject result ifTrue: [
			self operands = anObject operands ifTrue: [
				^ true. ]]].
	^ false. 
]

{ #category : #inspecting }
DRInstruction >> RBArrayNodenSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>
	
	^ originAST inspectionSourceCode
]

{ #category : #'users-definitions' }
DRInstruction >> addUser: anInstruction [
	
	users add: anInstruction
]

{ #category : #accessing }
DRInstruction >> basicBlock [

	^ basicBlock
]

{ #category : #accessing }
DRInstruction >> basicBlock: anObject [

	basicBlock := anObject
]

{ #category : #'users-definitions' }
DRInstruction >> clearUsers [

	users removeAll
]

{ #category : #copying }
DRInstruction >> copyToVariableIn: aDRPrimitiveIRGenerator inNode: aRBReturnNode [ 
	
	"I'm an already an instruction with a value. No need to do anything"
	^ self
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
DRInstruction >> hasUser: anInstruction [
	
	^ users includes: anInstruction
]

{ #category : #testing }
DRInstruction >> hash [

	^ self result hash bitXor: self operands hash
]

{ #category : #testing }
DRInstruction >> initialize [

	super initialize.
	operands := #().
	users := IdentitySet new.
]

{ #category : #inspecting }
DRInstruction >> inspectionDependencies: aBuilder [
	"<inspectorPresentationOrder: 0 title: 'Dependency Graph'>" 
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllDependencies collect: [ :c | | g |
		g := RSGroup new.
		g add: (RSCircle new
			size: 50;
			color: Color veryVeryLightGray;
			yourself).
		g add: (RSLabel new text: c label asString).
		g asShape
			@ RSDraggable;
			model: c;
			yourself ].
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #operands.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		doNotValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : #testing }
DRInstruction >> isCopy [
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

{ #category : #testing }
DRInstruction >> isNonConditionalJump [
	
	^ false
]

{ #category : #testing }
DRInstruction >> isPhiFunction [

	^ false
]

{ #category : #accessing }
DRInstruction >> label [

	^ self opcode, '(', self result name asString, ')'
]

{ #category : #interpretation }
DRInstruction >> lookupClass [
	
	^ result lookupClass 
]

{ #category : #accessing }
DRInstruction >> name [
	
	^ result name
]

{ #category : #accessing }
DRInstruction >> nonLocalUsers [

	^ self users reject: [ :i | i basicBlock = self basicBlock ]
]

{ #category : #printing }
DRInstruction >> opcode [
	
	^ self subclassResponsibility
]

{ #category : #accessing }
DRInstruction >> operand1 [
	
	^ operands at: 1
]

{ #category : #accessing }
DRInstruction >> operand2 [

	^ operands at: 2
]

{ #category : #operands }
DRInstruction >> operandAt: anInteger [
	
	^ operands at: anInteger
]

{ #category : #operands }
DRInstruction >> operandAt: anInteger put: aValue [ 
	
	operands at: anInteger put: aValue
]

{ #category : #accessing }
DRInstruction >> operands [
	^ operands
]

{ #category : #accessing }
DRInstruction >> operands: aCollection [ 

	operands asSet do: [ :each | each removeUser: self ].
	operands := aCollection asOrderedCollection.
	operands asSet do: [ :each | each addUser: self ]
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

	operands := operands copy.
	users := users copy.
	(result notNil and: [ result isPhysicalRegister not ]) ifTrue: [ 
		result := basicBlock controlFlowGraph allocateTemporaryRegister ].
	super postCopy.
]

{ #category : #printing }
DRInstruction >> prettyPrint [
	
	| print |
	print := result
		ifNil: [ '' ]
		ifNotNil: [ result name, ' := ' ].
	^ print, self opcode ,' ', (' ' join: (operands collect: [ :e | e result name ]))
]

{ #category : #printing }
DRInstruction >> printOn: aStream [

	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self prettyPrint.
	aStream nextPut: $)
]

{ #category : #removing }
DRInstruction >> removeFromCFG [

	basicBlock removeInstruction: self.
	self operands asSet do: [ :e | e removeUser: self ]
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
	
	users copy do: [ :instruction |
		instruction replaceOperand: self by: anotherInstruction ].

	operands asSet do: [ :instruction |
		instruction removeUser: self ].

	basicBlock replace: self by: anotherInstruction
]

{ #category : #operands }
DRInstruction >> replaceOperand: anOperand by: anotherOperand [

	anOperand removeUser: self.
	operands replaceAll: anOperand with: anotherOperand.
	anotherOperand addUser: self
]

{ #category : #operands }
DRInstruction >> replaceOperandIfDominated: myOperand by: anotherOperand [

	(self isDominatedBy: anotherOperand) ifFalse: [ ^ nil ].
	self replaceOperand: myOperand by: anotherOperand
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

{ #category : #accessing }
DRInstruction >> result [

	^ result
]

{ #category : #accessing }
DRInstruction >> result: anObject [

	result := anObject
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
	
	((({ sccp top. sccp bottom } includes: operand1) not
		and: [ ({ sccp top. sccp bottom } includes: operand2) not ])
			and: [ operand1 isConstant and: [ operand2 isConstant ]])
				ifTrue: [ 
					"Compute the lattice value and wrap it as a DR value"
					^ (self sccpOperateOnLattice: operand1 value and: operand2 value) asDRValue ].
	(operand1 = sccp bottom or: [operand2 = sccp bottom])
		ifTrue: [ ^ sccp bottom ].
	^ sccp top
]

{ #category : #SCCP }
DRInstruction >> sccpOperateOnLattice: operand and: operand2 [
	
	self subclassResponsibility
]

{ #category : #'constant folding' }
DRInstruction >> simpleConstantFold [
	
	self subclassResponsibility
]

{ #category : #accessing }
DRInstruction >> singleChild [

	^ self
]

{ #category : #replacing }
DRInstruction >> users [

	^ users
]

{ #category : #validation }
DRInstruction >> validate [

	(self operands allSatisfy: [ :op | op hasUser: self ]) ifFalse: [ 
		DRError signal:
			'Operand-user mismatch in instruction: ' , self asString ].
	(self users allSatisfy: [ :user | user hasOperand: self ]) ifFalse: [ 
		DRError signal:
			'User-operand mismatch in instruction: ' , self asString ].

	self isPhiFunction ifTrue: [ ^ self ].

	self operands do: [ :o | 
		(o isInstruction not or: [ 
			 self basicBlock isDominatedBy: o basicBlock ]) ifFalse: [ 
			DRError signal: 'Tenés cualquier operando en: ' , self asString ] ]
]

{ #category : #copying }
DRInstruction >> veryDeepCopyWith: deepCopier [
	deepCopier references at: origin ifAbsentPut: [nil].
	^ super veryDeepCopyWith: deepCopier
]

{ #category : #dependencies }
DRInstruction >> withAllDependencies [
	
	| dependencies |
	dependencies := OrderedCollection new.
	dependencies add: self.
	self operands do: [ :e |
		dependencies addAll: e withAllDependencies ].
	^ dependencies
]
