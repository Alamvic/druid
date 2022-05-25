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

	temporaries at: aName put: aValue
]
