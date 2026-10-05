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

{ #category : 'API' }
DRBlockIRGenerator >> interpretBlockClosure: aDRBlockClosure [
	"Create the initial basic block"

	| numArgs cfg |
	numArgs := aDRBlockClosure numberOfArguments.
	self numberOfArguments: numArgs.
	self interpretCode: aDRBlockClosure receiver: aDRBlockClosure.

	cfg := self ir.
	cfg numArgs: numArgs.

	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	cfg validate.	

	^ cfg
]

{ #category : 'testing' }
DRBlockIRGenerator >> isEmpty [

	^ self ir instructions last isNoop
	
]

{ #category : 'as yet unclassified' }
DRBlockIRGenerator >> temporariesListfrom: aNode [

	^ self scope allTempsWithOuterTemps 
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
	"Non-local return is attached to the compiled method (last scope)"
	| value |
	self assert: aRBReturnNode methodNode = self scope lastScope node.
	
	value := self visitOperand: aRBReturnNode value.
	self pushOperand: (currentBasicBlock return: value)
]

{ #category : 'visiting' }
DRBlockIRGenerator >> visitSelfNode: aRBVariableNode [ 

	self pushOperand: self receiver.

]
