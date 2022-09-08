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
DruidTestInterpreter >> ifZeroFailPrimitive [

	<numberOfArguments: 1>
	| reciver arg |
	reciver := self stackValue: 1.
	arg := self stackValue: 0.

	reciver = 0 ifTrue: [ ^ self primitiveFail ].
	self pop: 2 thenPush: 42
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> lazyAssert [

	<druidNotInline>
	self stackTop = 0 ifTrue: [ self lazyFail ]
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> lazyDouble: n [

	<druidNotInline>
	^ n * 2
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> lazyFail [

	<druidNotInline>
	self primitiveFail
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

{ #category : #comparisons }
DruidTestInterpreter >> primitiveAnd [

	<numberOfArguments: 0>
	| result |
	(self stackTop > 0 and: [ self stackTop > 10 ])
		ifTrue: [ result := 42 ]
		ifFalse: [ result := 10 ].

	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveAssertDeferredInline [

	<numberOfArguments: 0>
	| result |
	result := 42.
	self lazyAssert.
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveAssertIsIgnored [
	<numberOfArguments: 0>
	self assert: false.
	self pop: 1 thenPush: 17
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveBitShift [

	<numberOfArguments: 1>
	| receiver argument |
	receiver := self stackValue: 1.
	argument := self stackTop.
	self pop: 2 thenPush: (receiver bitShift: argument)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveBitXor [

	<numberOfArguments: 1>
	
	self pop: 2 thenPush: (self stackTop bitXor: (self stackValue: 1))
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveBranchingWithAndConditionOnDifferentVariables [

	<numberOfArguments: 1>
	| n m res |
	n := self stackValue: 1.
	m := self stackValue: 0.
	res := n ~= 2 & (m < 5)
		       ifTrue: [ 1 ]
		       ifFalse: [ 7 ].
	^ self pop: 1 thenPush: res
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveBranchingWithAndConditionOnSameVariable [

	<numberOfArguments: 0>
	| n res |
	n := self stackTop.
	res := n ~= 2 & (n < 5)
		       ifTrue: [ 1 ]
		       ifFalse: [ 7 ].
	^ self pop: 1 thenPush: res
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
DruidTestInterpreter >> primitiveBranchingWithOrConditionOnSameVariable [

	<numberOfArguments: 0>
	| n |
	n := self stackTop ~= 2 | (self stackTop < 5)
		     ifTrue: [ 1 ]
		     ifFalse: [ 7 ].
	^ self pop: 1 thenPush: n
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveBranchingWithTripleAnd [

	<numberOfArguments: 0>
	| n |
	n := self stackTop ~= 2 & (self stackTop < 5)
	     & (self stackTop anyMask: 1)
		     ifTrue: [ 1 ]
		     ifFalse: [ 7 ].
	^ self pop: 1 thenPush: n
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveBranchingWithTripleAndLeftAssociated [

	<numberOfArguments: 0>
	| n res |
	n := self stackTop.
	res := n ~= 2 & (n < 5) & (n anyMask: 1)
		       ifTrue: [ 1 ]
		       ifFalse: [ 7 ].
	^ self pop: 1 thenPush: res
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
DruidTestInterpreter >> primitiveCallingEmptyBlock [

	<numberOfArguments: 0>
	| value |
	value := [ ] value.
	value ifNil: [ value := 18 ].
	self pop: 1 thenPush: value
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
DruidTestInterpreter >> primitiveCascadedUint16AtPut [

	<numberOfArguments: 0>

	| addressToRead |
	addressToRead := self stackTop.
	objectMemory
		uint16At: addressToRead put: 1;
		uint16At: addressToRead + 2 put: 2
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveCaseOfOtherwiseValue [

	<numberOfArguments: 0>
	| result |
	result := self stackTop
		          caseOf: { 
				          ([ 0 ] -> [ 57 ]).
				          ([ 1 ] -> [ 42 ]) }
		          otherwise: [77].
	self pop: 1 thenPush: result
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
DruidTestInterpreter >> primitiveConditionWithObjectReference [

	<numberOfArguments: 0>

	| v |
	v := (objectMemory trueObject > self stackTop)
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
DruidTestInterpreter >> primitiveDecrementConstantArgument [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: self stackTop - 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDecrementConstantReceiver [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: 42 - self stackTop
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDivide [

	<numberOfArguments: 1>
	
	| result |
	result := (self stackValue: 1) / (self stackValue: 0).
	self pop: 2 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDivideByConstant [

	<numberOfArguments: 0>
	
	| result |
	result := (self stackValue: 0) / 2.
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDoubleDeferredInline [

	<numberOfArguments: 0>
	| result |
	result := self lazyDouble: self stackTop.
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveDoubleInnerBranching [

	<numberOfArguments: 0>
	| res t1 |
	t1 := 10.
	res := self stackTop > 10
		       ifTrue: [ | t2 |
			      t1 := self stackTop < 20
				       ifTrue: [ 5 ]
				       ifFalse: [ 17 ].
					t2 := self stackTop < 10
				       ifTrue: [ 15 ]
				       ifFalse: [ 27 ].
					t1 + t2 ]
		       ifFalse: [ 99 ].
	self pop: 1 thenPush: res + t1
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

{ #category : #comparisons }
DruidTestInterpreter >> primitiveEqualsThanValue [

	<numberOfArguments: 0>
	self pop: 1 thenPush: (self stackTop = 0)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveFailDeferredInline [

	<numberOfArguments: 0>
	| result |
	result := 42.
	self stackTop = 0 ifTrue: [ self lazyFail ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveFalseAsInteger [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: false asInteger
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveFalseIfTrue [

	<numberOfArguments: 0>
	| r bool |
	bool := false.
	r := bool ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveFalseNot [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: false not
]

{ #category : #comparisons }
DruidTestInterpreter >> primitiveGreaterOrEqualThanValue [

	<numberOfArguments: 0>
	self pop: 1 thenPush: (self stackTop >= 0)
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

{ #category : #comparisons }
DruidTestInterpreter >> primitiveGreaterThanReferenceValue [

	<numberOfArguments: 0>
	self pop: 1 thenPush: (self stackTop > objectMemory trueObject)
]

{ #category : #comparisons }
DruidTestInterpreter >> primitiveGreaterThanValue [

	<numberOfArguments: 0>
	self pop: 1 thenPush: (self stackTop > 0)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIfFalseIfTrueReturningValue [

	<numberOfArguments: 0>
	| result |
	result := self stackTop = 0
		ifFalse: [ 57 ]
		ifTrue: [ 42 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIfTrueIfFalseAssigningValue [

	<numberOfArguments: 0>
	| result |
	self stackTop = 0
		ifTrue: [ result := 42 ]
		ifFalse: [ result := 57 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIfTrueIfFalseReturningValue [

	<numberOfArguments: 0>
	| result |
	result := self stackTop = 0
		ifTrue: [ 42 ]
		ifFalse: [ 57 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIfTrueStatement [

	<numberOfArguments: 0>
	| result |
	result := 10.
	self stackTop = 0
		ifTrue: [ result := 42 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveImplicitArgumentBitShiftLeft [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: (1 bitShift: self stackTop)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveImplicitBitShiftLeft [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: (self stackTop bitShift: 1)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveImplicitBitShiftRight [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: (self stackTop bitShift: -1)
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIncrementConstantArgument [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: self stackTop + 1
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIncrementConstantReceiver [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: 1 + self stackTop
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
DruidTestInterpreter >> primitiveIntegerDivide [

	<numberOfArguments: 1>
	
	| result |
	result := (self stackValue: 1) // (self stackValue: 0).
	self pop: 2 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIntegerDivideByConstant [

	<numberOfArguments: 0>
	
	| result |
	result := (self stackValue: 0) // 2.
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveIsIntegerObject [

	<numberOfArguments: 0>

	self pop: 1 thenPush: (objectMemory booleanObjectOf: (objectMemory isIntegerObject: self stackTop))
]

{ #category : #comparisons }
DruidTestInterpreter >> primitiveLessOrEqualThanValue [

	<numberOfArguments: 0>
	self pop: 1 thenPush: (self stackTop <= 0)
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

{ #category : #comparisons }
DruidTestInterpreter >> primitiveLessThanValue [

	<numberOfArguments: 0>
	self pop: 1 thenPush: (self stackTop < 0)
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
DruidTestInterpreter >> primitiveModByConstant [

	<numberOfArguments: 0>
	
	| result |
	result := (self stackValue: 0) \\ 2.
	self pop: 1 thenPush: result
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
DruidTestInterpreter >> primitiveMultiplyConstantArgument [

	<numberOfArguments: 0>
	self pop: 1 thenPush: (self stackValue: 0) * 2
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveMultiplyConstantReceiver [

	<numberOfArguments: 0>
	self pop: 1 thenPush: 2 * (self stackValue: 0)
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
DruidTestInterpreter >> primitiveNextUint16At [

	<numberOfArguments: 0>
	
	| aByte addressToRead |
	addressToRead := self stackTop.
	aByte := objectMemory uint16At: (addressToRead + 1).

	"pop the receiver, push the result"	
	self pop: 1 thenPush: aByte
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

{ #category : #comparisons }
DruidTestInterpreter >> primitiveOr [

	<numberOfArguments: 0>
	| result |
	(self stackTop < 5 or: [ self stackTop > 10 ])
		ifTrue: [ result := 42 ]
		ifFalse: [ result := 10 ].

	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveReturnFalse [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: objectMemory falseObject
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
DruidTestInterpreter >> primitiveSizeofusqInt [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: (self sizeof: #usqInt)
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
DruidTestInterpreter >> primitiveTrueIfTrue [

	<numberOfArguments: 0>
	| r bool |
	bool := true.
	r := bool ifTrue: [ 5 ] ifFalse: [11].
	self pop: 1 thenPush: r
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveTrueNot [

	<numberOfArguments: 0>
	
	self pop: 1 thenPush: true not
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
DruidTestInterpreter >> primitiveUint16AtPut [

	<numberOfArguments: 1>
	
	| addressToRead valueToStore |
	valueToStore := self stackTop.
	addressToRead := self stackValue: 1.
	objectMemory uint16At: addressToRead put: valueToStore
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
DruidTestInterpreter >> primitiveUint32AtPut [

	<numberOfArguments: 1>
	
	| addressToRead valueToStore |
	valueToStore := self stackTop.
	addressToRead := self stackValue: 1.
	objectMemory uint32At: addressToRead put: valueToStore
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
DruidTestInterpreter >> primitiveUint8AtPut [

	<numberOfArguments: 1>
	
	| addressToRead valueToStore |
	valueToStore := self stackTop.
	addressToRead := self stackValue: 1.
	objectMemory uint8At: addressToRead put: valueToStore
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
DruidTestInterpreter >> primitiveWithIfNilIfNotNilStatement [

	<numberOfArguments: 0>
	| t result |
	t := self stackTop > 0
		ifTrue: [ nil ]
		ifFalse: [ 1 ].
	t
		ifNil: [ result := 17 ]
		ifNotNil: [ result := 42 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithIfNotNilIfNilStatement [

	<numberOfArguments: 0>
	| t result |
	t := self stackTop > 0
		ifTrue: [ nil ]
		ifFalse: [ 1 ].
	t
		ifNotNil: [ result := 42 ]
		ifNil: [ result := 17 ].
	self pop: 1 thenPush: result
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithIfNotNilIfNilStatementWithArgument [

	<numberOfArguments: 0>
	| t result |
	t := self stackTop > 0
		ifTrue: [ nil ]
		ifFalse: [ 42 ].
	t
		ifNotNil: [ :arg | result := arg ]
		ifNil: [ result := 17 ].
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
DruidTestInterpreter >> primitiveWithNonCompilableBranch [

	<numberOfArguments: 0>
	
	| r |
	r := self stackTop > 0
		ifTrue: [ 11 ]
		ifFalse: [ self nonCompilableMethod ].
	self pop: 1 thenPush: r.
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithOrBlock [

	<numberOfArguments: 0>
	| t |
	t := (self stackTop = 1 or: [ self stackTop = 2 ])
		ifTrue: [ 17 ]
		ifFalse: [ 42 ].
	self pop: 1 thenPush: t
]

{ #category : #'as yet unclassified' }
DruidTestInterpreter >> primitiveWithSequentialExitPoint [

	<numberOfArguments: 0>
	
	| r |
	r := 17.
	
	"This exit point makes the whole sequence non compilable.
	This primitive always fails and all is dead code"
	self nonCompilableMethod.
	self pop: 1 thenPush: r.
]
