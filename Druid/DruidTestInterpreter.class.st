Class {
	#name : #DruidTestInterpreter,
	#superclass : #CogVMSimulatorLSB,
	#category : #'Druid-Tests'
}

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> constant [

	^ 73
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> emptyPrimitive [

	<numberOfArguments: 0>
	"Do nothing"
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> emptyPrimitiveWithArguments [

	<numberOfArguments: 1>
	"Do nothing"
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> methodWithEarlyReturn [

	self stackTop = 0 ifTrue: [ ^ 42 ].
	^ 57
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> methodWithTwoEarlyReturns [

	self stackTop = 0
		ifTrue: [ ^ 42 ]
		ifFalse: [ ^ 57 ].

	"Dead code"
	^ 16rFFFF
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingMethodReturningConstant [

	<numberOfArguments: 0>

	self pop: 1 thenPush: self constant
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingMethodWithEarlyReturn [

	<numberOfArguments: 0>

	self pop: 1 thenPush: self methodWithEarlyReturn
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingMethodWithTwoEarlyReturns [

	<numberOfArguments: 0>

	self pop: 1 thenPush: self methodWithTwoEarlyReturns
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveConditionWithAnyMask [

	<numberOfArguments: 0>

	| v |
	v := (self stackTop anyMask: 1)
		ifTrue: [ 1 ]
		ifFalse: [ 2 ].
	
	self pop: 1 thenPush: v
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveReturnOne [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveReturnOneWithArguments [

	<numberOfArguments: 2>
	
	self pop: 3 "2 args + 1 receiver" thenPush: 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveReturnTrue [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: objectMemory trueObject
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithIfAssigningValue [

	<numberOfArguments: 0>
	| result |
	self stackTop = 0
		ifTrue: [ result := 42 ]
		ifFalse: [ result := 57 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithIfReturningValue [

	<numberOfArguments: 0>
	| result |
	result := self stackTop = 0
		ifTrue: [ 42 ]
		ifFalse: [ 57 ].
	self pop: 1 thenPush: result
]
