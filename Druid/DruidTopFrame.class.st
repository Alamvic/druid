Class {
	#name : #DruidTopFrame,
	#superclass : #Object,
	#instVars : [
		'result',
		'temporaryFrame',
		'executionState'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidTopFrame >> executionState [
	^ executionState
]

{ #category : #accessing }
DruidTopFrame >> executionState: aDRExecutionState [
	
	executionState := aDRExecutionState 
]

{ #category : #accessing }
DruidTopFrame >> returnValue [
	
	^ DRNullValue new
]
