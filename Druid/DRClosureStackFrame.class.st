Class {
	#name : #DRClosureStackFrame,
	#superclass : #DRStackFrame,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRClosureStackFrame >> homeFrame [
	
	^ receiver outerContext homeFrame
]
