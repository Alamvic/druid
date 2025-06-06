Class {
	#name : 'DRExecutionState',
	#superclass : 'Object',
	#instVars : [
		'vmState',
		'executionStack',
		'frameState'
	],
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'accumulating' }
DRExecutionState >> addTo: aDRExecutionState [ 
	
	"Add my own state to the execution state in parameter"
	vmState addTo: aDRExecutionState vmState.
	executionStack addTo: aDRExecutionState executionStack
]

{ #category : 'stack access' }
DRExecutionState >> baseFrame [
	
	^ executionStack baseFrame
]

{ #category : 'accessing' }
DRExecutionState >> executionStack [

	^ executionStack
]

{ #category : 'accessing' }
DRExecutionState >> executionStack: anObject [

	executionStack := anObject
]

{ #category : 'testing' }
DRExecutionState >> isNotSimplified [
	
	^ vmState isNotSimplified and: [ executionStack isNotSimplified ]
]

{ #category : 'accessing' }
DRExecutionState >> lastStackInstruction [

	^ self vmState lastStackInstruction
]

{ #category : 'accessing' }
DRExecutionState >> lastStackInstruction: aDRStackInstruction [

	self vmState lastStackInstruction: aDRStackInstruction
]

{ #category : 'frames' }
DRExecutionState >> popFrame [
	
	^ executionStack popFrame
]

{ #category : 'stack management' }
DRExecutionState >> popUpTo: aDRStackFrame [ 
	
	[self topFrame mirrorFrame = aDRStackFrame mirrorFrame]
		whileFalse: [ self popFrame ]
]

{ #category : 'copying' }
DRExecutionState >> popVM [

	^ self vmState pop
]

{ #category : 'copying' }
DRExecutionState >> postCopy [
	super postCopy.
	vmState := vmState copy.
	executionStack := executionStack copy.
]

{ #category : 'vm state' }
DRExecutionState >> primitiveFailureCode [
	
	^ vmState primitiveFailureCode
]

{ #category : 'accessing - vm state' }
DRExecutionState >> primitiveFailureCode: anObject [

	vmState primitiveFailureCode: anObject
]

{ #category : 'frames' }
DRExecutionState >> pushFrame: aDRStackFrame [ 
	
	executionStack pushFrame: aDRStackFrame
]

{ #category : 'copying' }
DRExecutionState >> pushVM: aValue [

	self vmState push: aValue
]

{ #category : 'simplification' }
DRExecutionState >> simplify [
	
	vmState simplify.
	frameState simplify.
]

{ #category : 'frames' }
DRExecutionState >> topFrame [
	
	^ executionStack topFrame
]

{ #category : 'copying' }
DRExecutionState >> topVM [

	^ self vmState top
]

{ #category : 'copying' }
DRExecutionState >> valueVM: offset [
	"0-based offset relateive to top.
	0 is top, 1 is after top and so on"

	^ self vmState stackOperandAt: offset + 1
]

{ #category : 'vm stack' }
DRExecutionState >> vmOperandStack [
	
	^ vmState operandStack
]

{ #category : 'accessing' }
DRExecutionState >> vmState [

	^ vmState
]

{ #category : 'accessing' }
DRExecutionState >> vmState: anObject [

	vmState := anObject
]
