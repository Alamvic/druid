Class {
	#name : 'DRBlockIRGenerator',
	#superclass : 'DRMethodIRGenerator',
	#instVars : [
		'variableFrame'
	],
	#category : 'Druid-Opal-Compiler',
	#package : 'Druid-Opal',
	#tag : 'Compiler'
}

{ #category : 'visiting' }
DRBlockIRGenerator >> finishCodeInterpretation: lastFrame [
	
	lastFrame returnedValue isReturn ifFalse: [ 
		currentBasicBlock blockReturn: lastFrame returnedValue.
	]
	
]

{ #category : 'testing' }
DRBlockIRGenerator >> isEmpty [

	^ self ir instructions last isNoop
	
]

{ #category : 'as yet unclassified' }
DRBlockIRGenerator >> temporariesListfrom: aNode [

	^ self scope allTempsWithOuterTemps 
]

{ #category : 'visiting' }
DRBlockIRGenerator >> variableFrame [

	^ variableFrame 
]

{ #category : 'accessing' }
DRBlockIRGenerator >> variableFrame: aDRStackFrame [

	variableFrame := aDRStackFrame
]

{ #category : 'accessing' }
DRBlockIRGenerator >> visitArgumentVariableNode: aRBVariableNode [
	"TODO: Repeat code"

	| loadArg varName scope |
	varName := aRBVariableNode name.
	scope := self lookupScopeDefiningVariable: aRBVariableNode.

	loadArg := self
		           addInstructionFrom: aRBVariableNode
		           instructionKind: DRLoadArgument
		           operands: { aRBVariableNode variable index asDRValue }.

	loadArg argName: varName. "Is it required?"
	loadArg scope: scope.

	^ loadArg
]

{ #category : 'visiting' }
DRBlockIRGenerator >> visitReturnNode: aRBReturnNode [

	| value |
	
	value := self visitOperand: aRBReturnNode value.

	self pushOperand:(currentBasicBlock return: value).

]

{ #category : 'visiting' }
DRBlockIRGenerator >> visitSelfNode: aRBVariableNode [ 

	self pushOperand: self receiver.

]
