Class {
	#name : #DRFrameState,
	#superclass : #Object,
	#instVars : [
		'builder',
		'operandStack',
		'temporaries',
		'outerFrameState',
		'frame'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #merging }
DRFrameState >> addTemporaryNamed: aString withValue: aValue [

	| temporary |
	temporary := temporaries
		at: aString
		ifAbsentPut: [ builder instructionFactory phiWithVariables: OrderedCollection new ].
	
	"If the temporary did not change, do nothing"
	temporary == aValue ifTrue: [ ^ self ].
	
	"Block closures are special and we do not handle polymorphic blocks.
	If the temporary is a block closure, override the phi function.
	Avoid overriding block closures too"
	temporary isDRBlockClosure ifTrue: [ 
		self error: 'cannot override block closures in the stack' ].
	
	aValue isDRBlockClosure ifTrue: [
		^ self writeTemporary: aString value: aValue ].
	
	"Otherwise, accumulate it"
	temporary addValue: aValue
]

{ #category : #merging }
DRFrameState >> addTo: aDRFrameState [ 
	
	"If we are not in the same frame, this may be a non local return.
	Look to merge my outer frame instead"
	self frame == aDRFrameState frame ifFalse: [ 
		^ (self includesFrame: aDRFrameState frame)
			ifTrue: [ outerFrameState addTo: aDRFrameState ]
			ifFalse: [ self addTo: aDRFrameState outerFrameState ] ].
	
	"Add my own state to the execution state in parameter"
	aDRFrameState temporaryNames do: [ :e | | valueToMerge |
		valueToMerge := temporaries at: e ifAbsent: [ DRNullValue new ].
		aDRFrameState addTemporaryNamed: e withValue: valueToMerge.
	].

	"Halt in these cases because we need to implement them :)"
	self haltIf: [ operandStack notNil ].
	outerFrameState ifNotNil: [ 
		outerFrameState addTo: aDRFrameState outerFrameState ]
]

{ #category : #accessing }
DRFrameState >> builder [

	^ builder
]

{ #category : #accessing }
DRFrameState >> builder: anObject [

	builder := anObject
]

{ #category : #merging }
DRFrameState >> definePhiPlaceholders [

	self definePhisWith: [ 
		builder instructionFactory phiWithVariables: OrderedCollection new ]
]

{ #category : #merging }
DRFrameState >> definePhisWith: aBlock [

	frame definedTemporaryNames do: [ :e | 
		self
			writeTemporary: e
			value: aBlock value ]
]

{ #category : #merging }
DRFrameState >> defineRecursivePhiPlaceholders [

	self defineRecursivePhisWith: [ 
		builder instructionFactory phiWithVariables: OrderedCollection new ]
]

{ #category : #merging }
DRFrameState >> defineRecursivePhisWith: aBlock [

	frame definedTemporaryNames do: [ :e | 
		self
			writeTemporary: e
			value: aBlock value ].

	outerFrameState ifNotNil: [ outerFrameState definePhisWith: aBlock ]
]

{ #category : #temporaries }
DRFrameState >> defineVariableNamed: aString [ 
	
	self writeTemporary: aString value: DRNullValue new
]

{ #category : #accessing }
DRFrameState >> frame [
	^ frame
]

{ #category : #accessing }
DRFrameState >> frame: aFrame [

	frame := aFrame
]

{ #category : #lookup }
DRFrameState >> frameDefining: aString [ 
	
	(temporaries includesKey: aString)
		ifTrue: [ ^ self ].
	^ outerFrameState frameDefining: aString
]

{ #category : #testing }
DRFrameState >> includesFrame: aFrame [
	
	frame == aFrame ifTrue: [ ^ true ].
	outerFrameState ifNil: [ ^ false ].
	^ outerFrameState includesFrame: aFrame
]

{ #category : #accessing }
DRFrameState >> initialize [

	super initialize.
	temporaries := Dictionary new
]

{ #category : #installation }
DRFrameState >> install [
	
	"Set myself and my hierarchy of states to our corresponding frames"
	frame frameState: self.
	outerFrameState ifNotNil: [ outerFrameState install ]
]

{ #category : #testing }
DRFrameState >> isNotSimplified [
	
	^ (temporaries allSatisfy: [ :e | 
		e isPhiFunction or: [ e isDRBlockClosure ] ])
			or: [ outerFrameState notNil
				and: [ outerFrameState isNotSimplified ] ]
]

{ #category : #accessing }
DRFrameState >> operandStack [

	^ operandStack
]

{ #category : #accessing }
DRFrameState >> operandStack: anObject [

	operandStack := anObject
]

{ #category : #accessing }
DRFrameState >> outerFrameState [
	
	^ outerFrameState
]

{ #category : #accessing }
DRFrameState >> outerFrameState: aDRFrameState [ 
	outerFrameState := aDRFrameState
]

{ #category : #copying }
DRFrameState >> postCopy [

	super postCopy.
	temporaries := temporaries copy.
	outerFrameState := outerFrameState copy
]

{ #category : #simplification }
DRFrameState >> simplify [
	
	temporaries keysAndValuesDo: [ :name :value | | newValue |
		newValue := value simplify.
		newValue ~~ value ifTrue: [
			self writeTemporary: name value: newValue.
			value replaceUsesBy: newValue ] ].
	
	outerFrameState ifNotNil: [ outerFrameState simplify ]
]

{ #category : #accessing }
DRFrameState >> temporaries [

	^ temporaries
]

{ #category : #accessing }
DRFrameState >> temporaries: anObject [

	temporaries := anObject
]

{ #category : #accessing }
DRFrameState >> temporaryAt: aName [

	^ temporaries
		at: aName
		ifAbsent: [ self outerFrameState temporaryAt: aName ]
]

{ #category : #accessing }
DRFrameState >> temporaryAt: aName put: aValue [

	(self frameDefining: aName) writeTemporary: aName value: aValue
]

{ #category : #accessing }
DRFrameState >> temporaryNames [
	
	^ temporaries keys
]

{ #category : #temporaries }
DRFrameState >> writeTemporary: aString value: aValue [
	
	self haltIf: [ aValue isPhiFunction and: [ aValue operands anySatisfy: [ :op | op isDRBlockClosure ] ] ].
	temporaries at: aString put: aValue
]
