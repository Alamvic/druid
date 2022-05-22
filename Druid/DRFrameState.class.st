Class {
	#name : #DRFrameState,
	#superclass : #Object,
	#instVars : [
		'operandStack',
		'temporaries'
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
