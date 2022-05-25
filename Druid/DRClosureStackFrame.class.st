Class {
	#name : #DRClosureStackFrame,
	#superclass : #DRStackFrame,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRClosureStackFrame >> homeFrame [
	
	^ receiver outerContext homeFrame
]

{ #category : #'instance creation' }
DRClosureStackFrame >> newFrameState [

	^ super newFrameState
		outerFrameState: self outerFrame frameState;
		yourself
]

{ #category : #'instance creation' }
DRClosureStackFrame >> outerFrame [
	
	^ receiver outerContext
]
