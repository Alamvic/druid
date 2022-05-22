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
DruidTopFrame >> executionState: aDRExecutionState [
	
	executionState := aDRExecutionState 
]

{ #category : #accessing }
DruidTopFrame >> push: aDRPhiFunction [ 

	result := aDRPhiFunction
]

{ #category : #accessing }
DruidTopFrame >> returnValue [
	
	^ DRNullValue new
]

{ #category : #accessing }
DruidTopFrame >> temporaryFrame [
	
	^ temporaryFrame ifNil: [ temporaryFrame := Dictionary new ]
]

{ #category : #accessing }
DruidTopFrame >> temporaryFrame: aDictionary [
	
	temporaryFrame := aDictionary
]
