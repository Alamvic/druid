Class {
	#name : #DRPop,
	#superclass : #DRStackEffect,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRPop >> acceptVisitor: aVisitor [

	^ aVisitor visitPop: self
]

{ #category : #testing }
DRPop >> isPop [
	
	^ true
]

{ #category : #testing }
DRPop >> isUnspilled [
	
	^ operands second value
]

{ #category : #accessing }
DRPop >> numberOfPoppedElements [
	
	^ self operand1 value
]

{ #category : #printing }
DRPop >> opcode [
	
	^ 'Pop'
]

{ #category : #SCCP }
DRPop >> sccpLatticeValueFor: sccp [ 
	
	"We know that we know nothing about what is in the stack"
	^ sccp bottom
]

{ #category : #SCCP }
DRPop >> size [
	"Popped memory size"
	
	^ 8 "bytes"
]

{ #category : #accessing }
DRPop >> stackDelta [
	
	"If I don't pop, my dependants should add one to their stack indexes"
	^ 1
]

{ #category : #testing }
DRPop >> stackDepth [
	"Return the stack depth left on the stack after applying this operation.
	Pop will remove one element from the stack depth of the previous stack instruction"

	stackDependencies ifEmpty: [ ^ -1 ].
	^ stackDependencies anyOne stackDepth - 1
]
