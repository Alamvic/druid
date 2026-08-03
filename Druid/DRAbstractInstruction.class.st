Class {
	#name : 'DRAbstractInstruction',
	#superclass : 'DRValue',
	#instVars : [
		'basicBlock'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRAbstractInstruction >> basicBlock [

	^ basicBlock
]

{ #category : 'accessing' }
DRAbstractInstruction >> basicBlock: anObject [

	basicBlock := anObject
]

{ #category : 'testing' }
DRAbstractInstruction >> isArithmeticInstruction [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isBytecodeJump [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isCall [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isInterpreterReference [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isLoadStackPointer [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isLoadStackValue [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isNegate [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isPrimitiveFail [
	
	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isScopedInstruction [

	^ false
]

{ #category : 'testing' }
DRAbstractInstruction >> isStackEffect [
	
	^ false
]

{ #category : 'accessing' }
DRAbstractInstruction >> nextInstruction [
	
	^ self basicBlock instructionAfter: self
]

{ #category : 'replacing' }
DRAbstractInstruction >> replaceBy: anotherInstruction [
	
	self = anotherInstruction ifTrue: [ ^ self ].

	basicBlock replace: self by: anotherInstruction.
	
	self replaceUsesBy: anotherInstruction.

	self operands asSet do: [ :instruction |
		instruction removeUser: self ].
	
	^ anotherInstruction
]

{ #category : 'replacing' }
DRAbstractInstruction >> replaceByMaybeValue: aDRValue [

	aDRValue isInstruction ifTrue: [ ^ self replaceBy: aDRValue ].

	^ self replaceBy: (DRCopy operands: { aDRValue } result: self result)
]

{ #category : 'translating' }
DRAbstractInstruction >> rtlArgumentsForAbsoluteAddressForGenerator: generator [
	"We are translating ourselves to a Base+offset, where offset=0"

	"In cogit offset comes before base"
	^ {
		  (RBLiteralValueNode value: 0).
		  (RBVariableNode named: (generator allocateVariable: self result)) }
]

{ #category : 'translating' }
DRAbstractInstruction >> rtlOperandQualifierForAbsoluteAddress [
	
	^ #'M64:r:'
]

{ #category : 'validation' }
DRAbstractInstruction >> validate [

	self validateDependenciesAreUsedByMyself.
	self validateUsersUseMyself.

	self operands do: [ :o |
		(o isInstruction not or: [
			 (self isDominatedBy: o) or: [
				 "this is to handle the case we have different CFG, block is interpreted with a new CFG"
				 self controlFlowGraph ~= o controlFlowGraph ] ]) ifFalse: [
			Error signal:
				'Invalid operand: ' , o asString , ' in: ' , self asString ] ]
]

{ #category : 'validation' }
DRAbstractInstruction >> validateDependenciesAreUsedByMyself [

	self dependencies do: [ :op | 
		(op hasUser: self) ifFalse: [ 
			DRError signal:
				'Dependency-user mismatch between: ' , self asString , ' and: '
				, op asString ] ]
]

{ #category : 'validation' }
DRAbstractInstruction >> validateUsersUseMyself [

	self users do: [ :user | 
		(user hasDependency: self) ifFalse: [ 
			DRError signal:
				'User-operand mismatch between: ' , user asString , ' and: '
				, self asString ] ]
]
