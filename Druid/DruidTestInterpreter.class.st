Class {
	#name : #DruidTestInterpreter,
	#superclass : #CogVMSimulatorLSB,
	#category : #'Druid-Tests'
}

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> basicInlineMethod [

	^ self lazyDouble: 42
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> methodWithEarlyReturn: n [

	n = 0 ifTrue: [ ^ 42 ].
	^ 57
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> methodWithThreeNonLocalReturn [

	true
		ifTrue: [ ^ 2 ]
		ifFalse: [ ^ 3 ].
	^ 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveAsInteger [

	<numberOfArguments: 0>
	"Transform the integer argument as float.
	Then read its raw representation and push that to the stack."
	self pop: 1 thenPush: self stackTop asInteger
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingMethodWithEarlyReturn [

	<numberOfArguments: 0>

	self pop: 1 thenPush: (self methodWithEarlyReturn: self stackTop)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveInnerBranchingWithDeadBranch [

	<numberOfArguments: 0>
	| res |
	res := self stackTop > 10
		       ifTrue: [ 
			       self stackTop < 5
				       ifTrue: [ 123 "Dead" ]
				       ifFalse: [ 17 ] ]
		       ifFalse: [ 99 ].
	self pop: 1 thenPush: res
]
