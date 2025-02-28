Class {
	#name : 'DRExecutionStack',
	#superclass : 'Object',
	#instVars : [
		'stateStack',
		'mirrorStack'
	],
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'merging' }
DRExecutionStack >> addTo: aDRExecutionStack [ 
	
	self assert: mirrorStack = aDRExecutionStack mirrorStack.
	
	stateStack withIndexDo: [ :e :i |
		e addTo: (aDRExecutionStack frameAt: i)
	]
]

{ #category : 'accessing' }
DRExecutionStack >> at: index [

	^ stateStack at: index
]

{ #category : 'accessing' }
DRExecutionStack >> baseFrame [
	
	^ stateStack last
]

{ #category : 'enumerating' }
DRExecutionStack >> detectFrame: aFullBlockClosure [ 
	
	^ stateStack detect: aFullBlockClosure
]

{ #category : 'accessing' }
DRExecutionStack >> frameAt: anInteger [ 
	
	^ stateStack at: anInteger
]

{ #category : 'accessing' }
DRExecutionStack >> frameAt: index ifAbsent: aBlock [

	^ stateStack at: index ifAbsent: aBlock
]

{ #category : 'accessing' }
DRExecutionStack >> framesDo: aBlock [
	
	^ stateStack do: aBlock
]

{ #category : 'testing' }
DRExecutionStack >> ifEmpty: aFullBlockClosure [

	stateStack ifEmpty: aFullBlockClosure
]

{ #category : 'accessing' }
DRExecutionStack >> indexOfFrame: aDRStackFrame [ 
	
	^ stateStack indexOf: aDRStackFrame
]

{ #category : 'initialization' }
DRExecutionStack >> initialize [

	super initialize.
	mirrorStack := Stack new.
	stateStack := Stack new
]

{ #category : 'accessing' }
DRExecutionStack >> mirrorStack [
	^ mirrorStack
]

{ #category : 'accessing' }
DRExecutionStack >> popFrame [
	
	mirrorStack pop.
	^ stateStack pop
]

{ #category : 'copying' }
DRExecutionStack >> postCopy [

	super postCopy.
	mirrorStack := mirrorStack copy.
	
	stateStack withIndexDo: [ :e :i |
		self haltIf: [(mirrorStack at: i) ~~ e mirrorFrame] ].
	stateStack := stateStack collect: [ :frame | frame copy ].
	stateStack withIndexDo: [ :e :i |
		self haltIf: [(mirrorStack at: i) ~~ e mirrorFrame] ]
	
]

{ #category : 'frames' }
DRExecutionStack >> pushFrame: aDRStackStack [

	mirrorStack push: aDRStackStack mirrorFrame.
	stateStack push: aDRStackStack
]

{ #category : 'frames' }
DRExecutionStack >> pushMergeFrame: aDRStackStack [

	mirrorStack push: aDRStackStack mirrorFrame.
	stateStack push: aDRStackStack
]

{ #category : 'accessing' }
DRExecutionStack >> size [

	^ stateStack size
]

{ #category : 'accessing' }
DRExecutionStack >> topFrame [
	
	^ stateStack top
]
