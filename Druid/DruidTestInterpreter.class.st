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
DruidTestInterpreter >> failingPrimitive2 [
	<numberOfArguments: 0>
	self primitiveFail
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> failingPrimitiveWithArg [
	<numberOfArguments: 1>
	self primitiveFailFor: self stackTop
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> failingSuccess [
	<numberOfArguments: 0>
	self success: false
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> methodWithEarlyReturn [

	self stackTop = 0 ifTrue: [ ^ 42 ].
	^ 57
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> methodWithNonLocalReturn [

	[ :arg | ^ arg ] value: 17.
	^ 1
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
DruidTestInterpreter >> multipleReturnsWithDeadPath [

	(self stackTop anyMask: 2) ifTrue: [ ^ 1 ].

	self stackTop <= 10
		ifTrue: [ ^ 2 ]
		ifFalse: [ ^ 3 ]
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> nonCompilableMethod [

	<druidExitPoint>
	^ 42
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> passingSuccess [
	<numberOfArguments: 0>
	self success: true.
	self pop: 1 thenPush: 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveAccessTempAfterCall [

	| temp |
	<numberOfArguments: 0>
	temp := 17.
	self constant.
	self pop: 1 thenPush: temp
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveAssertIsIgnored [
	<numberOfArguments: 0>
	self assert: false.
	self pop: 1 thenPush: 17
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveBranchingWithAssigments [

	<numberOfArguments: 0>
	| a b res |
	a := 17.
	b := 42.
	res := self stackTop > 0
		ifTrue: [ a ]
		ifFalse: [ b ].
	self pop: 1 thenPush: res
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingBlockReadingExternalTemp [

	<numberOfArguments: 0>
	| t |
	t := 17.
	self pop: 1 thenPush: [ t ] value
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingBlockReturningArgument [

	<numberOfArguments: 0>

	self pop: 1 thenPush: ([ :arg | arg ] value: 17)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingBlockReturningConstant [

	<numberOfArguments: 0>

	self pop: 1 thenPush: [ 17 ] value
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingBlockWithNonLocalReturn [

	<numberOfArguments: 0>

	self pop: 1 thenPush: self methodWithNonLocalReturn
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCallingBlockWritingExternalTemp [

	<numberOfArguments: 0>
	| t |
	[ t := 17 ] value.
	self pop: 1 thenPush: t
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
DruidTestInterpreter >> primitiveChangingVariableInOnePath [

	<numberOfArguments: 0>
	| res res2 |
	res := 10.
	res2 := 0.
	self stackTop > 0 ifTrue: [ res := 42 ] ifFalse: [res2 := 1].
	self pop: 1 thenPush: res + res2
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
DruidTestInterpreter >> primitiveDNA [

	<numberOfArguments: 0>
	| res res2 res3 |
	res := self stackTop > 10
		       ifTrue: [ 1 ]
		       ifFalse: [ 99 ].
	res2 := self stackTop > 0
		        ifTrue: [ 5 ]
		        ifFalse: [ 17 ].
	res3 := self stackTop > 7
		        ifTrue: [ 15 ]
		        ifFalse: [ 42 ].
		
	self pop: 1 thenPush: res + res2 + res3
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDNA2 [

	<numberOfArguments: 0>
	| res res2 res3 |
	res := self stackTop > 10
		       ifTrue: [ 1 ]
		       ifFalse: [ 99 ].
	res2 := self stackTop = 100
		        ifTrue: [ 5 ]
		        ifFalse: [ 17 ].
	res3 := self stackTop > 7
		        ifTrue: [ 15 ]
		        ifFalse: [ 42 ].
		
	self pop: 1 thenPush: res + res2 + res3
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDeadBranchWithError [

	<numberOfArguments: 0>
	1 > 0 ifFalse: [ self error: 'should not happen' ].
	
	self pop: 1 thenPush: 17
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDecrement [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: self stackTop - 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDivide [

	<numberOfArguments: 1>
	
	| result |
	result := (self stackValue: 1) // (self stackValue: 0).
	self pop: 2 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveEqualsThan [

	<numberOfArguments: 0>
	| r |
	r := self stackTop = 10 ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveEqualsThanInverted [
	"Put the constant as the receiver"
	<numberOfArguments: 0>
	| r |
	r := 10 = self stackTop ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
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
DruidTestInterpreter >> primitiveInnerBranching [

	<numberOfArguments: 0>
	| res |
	res := self stackTop > 10
		       ifTrue: [ 
			       self stackTop < 20
				       ifTrue: [ 5 ]
				       ifFalse: [ 17 ] ]
		       ifFalse: [ 99 ].
	self pop: 1 thenPush: res
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveInnerBranchingWithDeadBranch [

	<numberOfArguments: 0>
	| res |
	res := self stackTop > 10
		       ifTrue: [ 
			       self stackTop < 5
				       ifTrue: [ 5 "Dead" ]
				       ifFalse: [ 17 ] ]
		       ifFalse: [ 99 ].
	self pop: 1 thenPush: res
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveInnerBranchingWithDeadBranches [

	<numberOfArguments: 0>
	| res |
	res := self stackTop > 10
		       ifTrue: [ 
			       self stackTop < 5
				       ifTrue: [ 5 "Dead" ]
				       ifFalse: [ 17 ] ]
		       ifFalse: [ 
			       self stackTop > 15
				       ifTrue: [ 25 "Dead" ]
				       ifFalse: [ 27 ] ].
	self pop: 1 thenPush: res
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
DruidTestInterpreter >> primitiveLoopIncrementing [

	<numberOfArguments: 0>
	| result |
	result := 10.
	[result < 15] whileTrue: [ result := result + 1 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveLoopIncrementingGlobalState [

	<numberOfArguments: 0>
	self pop: 1 thenPush: 10.
	[self stackTop < 15] whileTrue: [
		self pop: 1 thenPush: self stackTop + 1 ]
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveMod [

	<numberOfArguments: 1>
	
	| result |
	result := (self stackValue: 1) \\ (self stackValue: 0).
	self pop: 2 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveMultiplePathsOnlyOneDead [

	<numberOfArguments: 0>
	| res res2 |
	res := self multipleReturnsWithDeadPath.

	res2 := (self stackTop anyMask: 3)
				ifTrue: [ 4 ]
				ifFalse: [ 5 ].

	self pop: 1 thenPush: res + res2
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
DruidTestInterpreter >> primitiveNegated [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: self stackTop negated
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveNotEqualsThan [

	<numberOfArguments: 0>
	| r |
	r := self stackTop ~= 10 ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveNotEqualsThanInverted [
	"Put the constant as the receiver"
	<numberOfArguments: 0>
	| r |
	r := 10 ~= self stackTop ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
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
DruidTestInterpreter >> primitiveSandclock [

	<numberOfArguments: 0>
	| res res2 |
	res := self stackTop > 10
		       ifTrue: [ 1 ]
		       ifFalse: [ 99 ].
	res2 := self stackTop > 0
		        ifTrue: [ 5 ]
		        ifFalse: [ 17 ].
		
	self pop: 1 thenPush: res + res2
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
DruidTestInterpreter >> primitiveToDo [

	<numberOfArguments: 0>
	| result |
	result := 1.
	1 to: self stackTop do: [ :i |
		result := result + 1
	].

	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveUint16At [

	<numberOfArguments: 0>
	
	| aByte addressToRead |
	addressToRead := self stackTop.
	aByte := objectMemory uint16At: addressToRead.

	"pop the receiver, push the result"	
	self pop: 1 thenPush: aByte
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveUint32At [

	<numberOfArguments: 0>
	
	| aByte addressToRead |
	addressToRead := self stackTop.
	aByte := objectMemory uint32At: addressToRead.

	"pop the receiver, push the result"	
	self pop: 1 thenPush: aByte
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveUint64At [

	<numberOfArguments: 0>
	
	| aByte addressToRead |
	addressToRead := self stackTop.
	aByte := objectMemory uint64At: addressToRead.

	"pop the receiver, push the result"	
	self pop: 1 thenPush: aByte
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveUint64AtPut [

	<numberOfArguments: 1>
	
	| addressToRead valueToStore |
	valueToStore := self stackTop.
	addressToRead := self stackValue: 1.
	objectMemory uint64At: addressToRead put: valueToStore
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveUint8At [

	<numberOfArguments: 0>
	
	| aByte addressToRead |
	addressToRead := self stackTop.
	aByte := objectMemory uint8At: addressToRead.

	"pop the receiver, push the result"	
	self pop: 1 thenPush: aByte
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
DruidTestInterpreter >> primitiveWithIfNotNilStatement [

	<numberOfArguments: 0>
	| t result |
	t := self stackTop > 0
		ifTrue: [ nil ]
		ifFalse: [ 1 ].
	result := 17.
	t ifNotNil: [ result := 42 ].
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

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithNonCompilableBranch [

	<numberOfArguments: 0>
	
	| r |
	r := self stackTop > 0
		ifTrue: [ 11 ]
		ifFalse: [ self nonCompilableMethod ].
	self pop: 1 thenPush: r.
]
