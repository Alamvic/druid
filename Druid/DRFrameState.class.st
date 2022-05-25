Class {
	#name : #DRFrameState,
	#superclass : #Object,
	#instVars : [
		'operandStack',
		'temporaries',
		'outerFrameState'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #temporaries }
DRFrameState >> defineVariableNamed: aString [ 
	
	temporaries at: aString put: DRNullValue new
]

{ #category : #lookup }
DRFrameState >> frameDefining: aString [ 
	
	(temporaries includesKey: aString)
		ifTrue: [ ^ self ].
	^ outerFrameState frameDefining: aString
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

{ #category : #temporaries }
DRFrameState >> writeTemporary: aString value: aValue [
	
	temporaries at: aString put: aValue
]
