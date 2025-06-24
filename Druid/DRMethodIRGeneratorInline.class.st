Class {
	#name : 'DRMethodIRGeneratorInline',
	#superclass : 'DRMethodIRGenerator',
	#instVars : [
		'scope'
	],
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
}

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> currentBasicBlock: aDRBasicBlock [

	currentBasicBlock := aDRBasicBlock
]

{ #category : 'scopes' }
DRMethodIRGeneratorInline >> ensureScope [

	^ scope := DRScope new
		           outerScope: self outerScope;
		           "id: self outerScope id + 1;"
		           "addCopiedVarsFromOuterScope: self outerScope;"
		           yourself
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	| lastBlock |
	lastBlock := currentBasicBlock.
	"Non-local returns can add new returns to any frame, pop all frames to resolve them"
	[ executionState executionStack size = 0 ] whileFalse: [ self popFrameMergingDeferredReturns ].
	currentBasicBlock := lastBlock.
	self mergeScope

]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> handleCFGScope: methodNode [

	"here to handle the scope with inlining"
]

{ #category : 'testing' }
DRMethodIRGeneratorInline >> isBaseMethodArgument: aRBVariableNode [

	"Here we check if the variable is accessing to a temporary (mainly arguments) from the compiled method.
	This change between accessing a variable from the method to inline and from the method where it is inlined."

	^ executionState baseFrame method = aRBVariableNode variable scope node
]

{ #category : 'scopes' }
DRMethodIRGeneratorInline >> mergeScope [

	self assert: self scope outerScope = self outerScope.

	"Rename to avoid collision"
	self scope tempVarNames do: [ :name |
		| suffix |
		suffix := self outerScope tempVarNames count: [ :outerName | outerName beginsWith: name ].
		self ir
			renameTempVar: name
			from: self scope
			into: name , suffix asString ].

	self ir mergeScope: self scope into: self outerScope.

	1 halt
]

{ #category : 'building' }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> outerScope [

	^ self ir scope
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> scope [

	^ scope
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> visitTemporaryVariableNode: aRBVariableNode [

	aRBVariableNode isTempVariable ifTrue: [
		| store temp |
		store := self topFrame
			         temporaryAt: aRBVariableNode name
			         withState: executionState.
		"store isStore ifFalse: [
			store := nil ]."
		temp := self
			        addInstructionFrom: aRBVariableNode
			        instructionKind: DRLoadTemporaryVariable
			        operands: {
					        aRBVariableNode name asDRValue.
					        store }.

		temp scope:
			(self scope lookupVarOrAddNewTemp: aRBVariableNode name).
		^ temp ].

	"Else, it is an argument"
	(self isBaseMethodArgument: aRBVariableNode)
		ifTrue: [
			| loadArg |
			loadArg := self
				           addInstructionFrom: aRBVariableNode
				           instructionKind: DRLoadArgument
				           operands: { aRBVariableNode variable index asDRValue }.
			loadArg argName: aRBVariableNode name.
			^ loadArg ]
		ifFalse: [
			| value |
			"If it is not accesssing a method argument, 
			then get the target instruction and push it"
			value := self temporaryAt: aRBVariableNode name.
			^ self pushOperand: value ]
]
