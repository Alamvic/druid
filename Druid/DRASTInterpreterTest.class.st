Class {
	#name : #DRASTInterpreterTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #helpers }
DRASTInterpreterTest >> newASTInterpreter [
	
	^ Druid new
]

{ #category : #'tests-specialcases' }
DRASTInterpreterTest >> testVisitBitShiftMessageWithConstantsCalculatesBitShift [

	| interpreter result left right |
	interpreter := self newASTInterpreter.
	left := interpreter irBuilder newConstant: 3.
	right := interpreter irBuilder newConstant: 2.
	
	interpreter
		pushNewContextWithMethod: (RBMethodNode new arguments: { RBArgumentNode named: 'test' })
		withReceiver: left
		withArguments: { right }.
	
	result := (RBMessageNode 
		receiver: RBSelfNode new
		selector: #<<
		arguments: { RBArgumentNode named: 'test' }) acceptVisitor: interpreter.
	
	self assert: result isConstant.
	self assert: result value equals: (3 << 2).
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

{ #category : #'tests-specialcases' }
DRASTInterpreterTest >> testVisitLocalFPInstanceVariableReturnsBasePointerRegister [

	| interpreter result |
	interpreter := self newASTInterpreter.
	result := (RBInstanceVariableNode named: #localFP) acceptVisitor: interpreter.
	
	self assert: result equals: DRPhisicalRegister basePointer.
]
