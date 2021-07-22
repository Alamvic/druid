Class {
	#name : #DruidTopFrame,
	#superclass : #Object,
	#instVars : [
		'result',
		'temporaryFrame'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DruidTopFrame >> push: aDRPhiFunction [ 

	result := aDRPhiFunction
]

{ #category : #accessing }
DruidTopFrame >> temporaryFrame [
	
	^ temporaryFrame ifNil: [ temporaryFrame := Dictionary new ]
]

{ #category : #accessing }
DruidTopFrame >> temporaryFrame: aDictionary [
	
	temporaryFrame := aDictionary
]
