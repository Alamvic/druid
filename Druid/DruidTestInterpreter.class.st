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
DruidTestInterpreter >> failingPrimitive [
	<numberOfArguments: 0>
	self success: false
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> failingPrimitive2 [
	<numberOfArguments: 0>
	self primitiveFail
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
DruidTestInterpreter >> primitiveConditionWithAnyMaskInverted [
	"Put the constant as the receiver"

	<numberOfArguments: 0>

	| v |
	v := (1 anyMask: self stackTop)
		ifTrue: [ 1 ]
		ifFalse: [ 2 ].
	
	self pop: 1 thenPush: v
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDecrement [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: self stackTop - 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveGreaterOrEqualsThan [

	<numberOfArguments: 0>
	| r |
	r := self stackTop >= 10 ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveGreaterOrEqualsThanInverted [
	"Put the constant as the receiver"
	<numberOfArguments: 0>
	| r |
	r := 10 >= self stackTop ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveGreaterThan [

	<numberOfArguments: 0>
	| r |
	r := self stackTop > 10 ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveGreaterThanInverted [
	"Put the constant as the receiver"

	<numberOfArguments: 0>
	| r |
	r := 10 > self stackTop ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIncrement [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: self stackTop + 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIsIntegerObject [

	<numberOfArguments: 0>

	self pop: 1 thenPush: (objectMemory booleanObjectOf: (objectMemory isIntegerObject: self stackTop))
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveLessOrEqualsThan [

	<numberOfArguments: 0>
	| r |
	r := self stackTop <= 10 ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveLessOrEqualsThanInverted [
	"Put the constant as the receiver"

	<numberOfArguments: 0>
	| r |
	r := 10 <= self stackTop ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveLessThan [

	<numberOfArguments: 0>
	| r |
	r := self stackTop < 10 ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveLessThanInverted [
	"Put the constant as the receiver"

	<numberOfArguments: 0>
	| r |
	r := 10 < self stackTop ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveMultiplyWithOverflow [

	<numberOfArguments: 1>
	
	| result |
	result := self
		multiplySmallInteger: (self stackValue: 1)
		withSmallInteger: (self stackValue: 0)
		ifOverflow: [ 99 ].
	self pop: 2 thenPush: result
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
DruidTestInterpreter >> primitiveSubWithOverflow [

	<numberOfArguments: 1>
	
	| result |
	result := self
		subSmallInteger: (self stackValue: 1)
		withSmallInteger: (self stackValue: 0)
		ifOverflow: [ 99 ].
	self pop: 2 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveSumWithOverflow [

	<numberOfArguments: 1>
	
	| result |
	result := self
		sumSmallInteger: (self stackValue: 1)
		withSmallInteger: (self stackValue: 0)
		ifOverflow: [ 99 ].
	self pop: 2 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithDeadCode [

	<numberOfArguments: 0>

	self stackTop = 0
		ifTrue: [ ^ self pop: 1 thenPush: 42 ]
		ifFalse: [ ^ self pop: 1 thenPush: 55 ].
	^ self pop: 1 thenPush: 10
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
