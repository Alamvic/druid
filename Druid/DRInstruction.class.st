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

{ #category : #'instance creation' }
DRInstruction class >> operands: aCollection result: aResultRegister [
	
	^ self new
		operands: aCollection;
		result: aResultRegister;
		yourself
]

{ #category : #testing }
DRInstruction >> = anObject [ 

	self class = anObject class ifTrue: [ 
		self result = anObject result ifTrue: [
			self operands =  anObject operands ifTrue: [
				^ true. ]]].
	^ false. 
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

{ #category : #copying }
DRInstruction >> copyToVariableIn: aDRPrimitiveIRGenerator inNode: aRBReturnNode [ 
	
	"I'm an already an instruction with a value. No need to do anything"
	^ self
]

{ #category : #testing }
DRInstruction >> initialize [

	super initialize.
	operands := #().
	users := OrderedCollection new.
]

{ #category : #testing }
DRInstruction >> isCopy [
	^ false
]

{ #category : #accessing }
DRInstruction >> isInstruction [

	^ true
]

{ #category : #testing }
DRInstruction >> isPhiFunction [

	^ false
]

{ #category : #accessing }
DRInstruction >> name [
	
	^ result name
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
DRInstruction >> operandAt: anInteger put: aValue [ 
	
	operands at: anInteger put: aValue
]

{ #category : #accessing }
DRInstruction >> operands [
	^ operands
]

{ #category : #accessing }
DRInstruction >> operands: aCollection [ 

	operands do: [ :each | each removeUser: self ].
	operands := aCollection.
	operands do: [ :each | each addUser: self ]
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
DRInstruction >> originAST: aNode [
	
	originAST := aNode
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

	basicBlock removeInstruction: self
]

{ #category : #'users-definitions' }
DRInstruction >> removeUser: anInstruction [
	
	users remove: anInstruction
]

{ #category : #replacing }
DRInstruction >> replaceBy: anotherInstruction [
	
	users do: [ :instruction |
		instruction replaceOperand: self by: anotherInstruction ].
	basicBlock replace: self by: anotherInstruction
]

{ #category : #'users-definitions' }
DRInstruction >> replaceOperand: anOperand by: anotherOperand [

	anOperand removeUser: self.
	operands replaceAll: anOperand with: anotherOperand.
	anotherOperand addUser: self.
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
				ifTrue: [ ^ self sccpOperateOnLattice: operand1 value and: operand2 value ].
	(operand1 = sccp bottom or: [operand2 = sccp bottom])
		ifTrue: [ ^ sccp bottom ].
	^ sccp top
]

{ #category : #SCCP }
DRInstruction >> sccpOperateOnLattice: operand and: operand2 [
	
	self subclassResponsibility
]

{ #category : #accessing }
DRInstruction >> singleChild [

	^ self
]
