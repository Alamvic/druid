Class {
	#name : #DRClosureStackFrame,
	#superclass : #DRStackFrame,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRClosureStackFrame >> homeFrame [

	^ receiver outerContext homeFrame
]

{ #category : #accessing }
DRClosureStackFrame >> homeFrameWithState: aState [
	
	^ (self outerFrameWithState: aState) homeFrameWithState: aState
]

{ #category : #'instance creation' }
DRClosureStackFrame >> outerFrame [

	^ receiver outerContext
]

{ #category : #'instance creation' }
DRClosureStackFrame >> outerFrameWithState: aState [

	^ aState executionStack detectFrame: [ :f | f mirrorFrame == self outerFrame mirrorFrame ]
]

{ #category : #printing }
DRClosureStackFrame >> printOn: stream [

	stream
		nextPutAll: self class name;
		nextPutAll: '(';
		print: method formattedCode;
		nextPutAll: ')'
]

{ #category : #accessing }
DRClosureStackFrame >> receiver [

	^ self homeFrame receiver
]

{ #category : #accessing }
DRClosureStackFrame >> receiverWithState: aState [

	^ (self homeFrameWithState: aState) receiver
]
