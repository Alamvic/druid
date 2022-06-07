Class {
	#name : #DruidStackFrame,
	#superclass : #Object,
	#instVars : [
		'receiver',
		'method',
		'irGenerator',
		'entryBasicBlock',
		'exitBasicBlock',
		'operandStack',
		'currentTemporaryFrame',
		'executionState'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidStackFrame >> executionState [
	^ executionState
]

{ #category : #accessing }
DruidStackFrame >> executionState: anExecutionState [
	
	executionState := anExecutionState
]

{ #category : #accessing }
DruidStackFrame >> homeFrame [
	
	^ self
]
