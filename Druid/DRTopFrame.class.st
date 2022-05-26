Class {
	#name : #DRTopFrame,
	#superclass : #Object,
	#instVars : [
		'result',
		'temporaryFrame',
		'executionState'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRTopFrame >> executionState [
	^ executionState
]

{ #category : #accessing }
DRTopFrame >> executionState: aDRExecutionState [
	
	executionState := aDRExecutionState 
]

{ #category : #accessing }
DRTopFrame >> frameState [
	
	^ DRFrameState new
]

{ #category : #accessing }
DRTopFrame >> push: aDRPhiFunction [ 

	result := aDRPhiFunction
]

{ #category : #accessing }
DRTopFrame >> returnValue [
	
	^ DRNullValue new
]

{ #category : #accessing }
DRTopFrame >> temporaryFrame [
	
	^ temporaryFrame ifNil: [ temporaryFrame := Dictionary new ]
]

{ #category : #accessing }
DRTopFrame >> temporaryFrame: aDictionary [
	
	temporaryFrame := aDictionary
]
