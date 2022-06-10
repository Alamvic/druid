Class {
	#name : #DruidClosureStackFrame,
	#superclass : #DruidStackFrame,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidClosureStackFrame >> homeFrame [
	
	^ receiver outerContext homeFrame
]
