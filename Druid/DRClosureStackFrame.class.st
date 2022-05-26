Class {
	#name : #DRClosureStackFrame,
	#superclass : #DRStackFrame,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRClosureStackFrame >> freshFrameState [

	^ super freshFrameState
		outerFrameState: self outerFrame executionState frameState copy;
		yourself
]

{ #category : #accessing }
DRClosureStackFrame >> homeFrame [
	
	^ receiver outerContext homeFrame
]

{ #category : #'instance creation' }
DRClosureStackFrame >> newFrameState [

	^ super newFrameState
		outerFrameState: self outerFrame newFrameState;
		yourself
]

{ #category : #'instance creation' }
DRClosureStackFrame >> outerFrame [
	
	^ receiver outerContext
]
