Class {
	#name : #DRInstruction,
	#superclass : #DRValue,
	#instVars : [
		'operands',
		'result',
		'order',
		'origin',
		'originAST'
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

{ #category : #testing }
DRInstruction >> isCopy [
	^ false
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

{ #category : #accessing }
DRInstruction >> operands [
	^ operands
]

{ #category : #accessing }
DRInstruction >> operands: aCollection [ 
	operands := aCollection
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

{ #category : #accessing }
DRInstruction >> result [

	^ result
]

{ #category : #accessing }
DRInstruction >> result: anObject [

	result := anObject
]

{ #category : #accessing }
DRInstruction >> simplify [
	
	^ self
]

{ #category : #accessing }
DRInstruction >> singleChild [

	^ self
]
