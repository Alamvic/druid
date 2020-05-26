Class {
	#name : #DRASTInterpreterTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #helpers }
DRASTInterpreterTest >> newASTInterpreter [
	
	^ Druid new
]

{ #category : #tests }
DRASTInterpreterTest >> testVisitGlobalVariableCreatesConstant [

	| interpreter result |
	interpreter := self newASTInterpreter.
	result := (RBGlobalNode new binding: #name -> 42) acceptVisitor: interpreter.
	
	self assert: result isConstant.
	self assert: result value equals: 42.
]

{ #category : #tests }
DRASTInterpreterTest >> testVisitInstanceVariableAccessesExistingValue [

	| interpreter result |
	interpreter := self newASTInterpreter.
	interpreter
		pushNewContextWithMethod: nil
		withReceiver: 17@2 "A point with x and y instance variables"
		withArguments: #().
	result := (RBInstanceVariableNode named: #x) acceptVisitor: interpreter.
	
	self assert: result equals: 17.
]

{ #category : #tests }
DRASTInterpreterTest >> testVisitLocalFPInstanceVariableReturnsBasePointerRegister [

	| interpreter result |
	interpreter := self newASTInterpreter.
	result := (RBInstanceVariableNode named: #localFP) acceptVisitor: interpreter.
	
	self assert: result equals: DRPhisicalRegister basePointer.
]
