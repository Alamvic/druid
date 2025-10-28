Class {
	#name : 'DRClosureStackFrame',
	#superclass : 'DRStackFrame',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'temporaries' }
DRClosureStackFrame >> frameDefining: aName withState: aState [

	^ (super frameDefining: aName withState: aState) ifNil: [
		  (self outerFrameWithState: aState)
			  frameDefining: aName
			  withState: aState ]
]

{ #category : 'accessing' }
DRClosureStackFrame >> homeFrame [

	^ receiver outerContext homeFrame
]

{ #category : 'accessing' }
DRClosureStackFrame >> homeFrameWithState: aState [
	
	^ (self outerFrameWithState: aState) homeFrameWithState: aState
]

{ #category : 'instance creation' }
DRClosureStackFrame >> outerFrame [

	^ receiver outerContext
]

{ #category : 'instance creation' }
DRClosureStackFrame >> outerFrameWithState: aState [

	^ aState executionStack detectFrame: [ :f | f mirrorFrame == self outerFrame mirrorFrame ]
]

{ #category : 'printing' }
DRClosureStackFrame >> printOn: stream [

	stream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: method formattedCode;
		nextPutAll: ')'
]

{ #category : 'accessing' }
DRClosureStackFrame >> receiver [

	^ self homeFrame receiver
]

{ #category : 'accessing' }
DRClosureStackFrame >> receiverWithState: aState [

	^ (self homeFrameWithState: aState) receiver
]

{ #category : 'temporaries' }
DRClosureStackFrame >> temporaryAt: aName put: aValue withState: aState [

	aState baseFrame = self ifFalse: [
		^ super temporaryAt: aName put: aValue withState: aState ].


	"We are compiling a block, temporary comes from temp vector. Simulates here"
	self writeTemporaryNamed: aName withValue: aValue
]

{ #category : 'temporaries' }
DRClosureStackFrame >> temporaryAt: aName withState: aState [

	aState baseFrame = self ifTrue: [
		(temporaries includesKey: aName) ifFalse: [ 1halt.
			"We are compiling a block, temporary comes from temp vector, not in this compilation scope"
			^ DRNullValue new ] ].

	^ super temporaryAt: aName withState: aState
]
