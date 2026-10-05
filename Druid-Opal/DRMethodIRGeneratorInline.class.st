Class {
	#name : 'DRMethodIRGeneratorInline',
	#superclass : 'DRMethodIRGenerator',
	#instVars : [
		'scope',
		'outerScope'
	],
	#category : 'Druid-Opal-Compiler',
	#package : 'Druid-Opal',
	#tag : 'Compiler'
}

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> currentBasicBlock: aDRBasicBlock [

	currentBasicBlock := aDRBasicBlock
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	| lastBlock |
	lastBlock := currentBasicBlock.
	"Non-local returns can add new returns to any frame, pop all frames to resolve them"
	[ executionState executionStack size = 0 ] whileFalse: [ self popFrameMergingDeferredReturns ].
	currentBasicBlock := lastBlock
]

{ #category : 'scopes' }
DRMethodIRGeneratorInline >> generateScope [

	scope := self newScope.
	self outerScope addInlinedScope: scope
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> handleCFGScope: methodNode [

	"here to handle the scope with inlining"
]

{ #category : 'initialization' }
DRMethodIRGeneratorInline >> initializeArgumentsNames: aDRCode with: argumentsValues [

	| argumentNames |
	super initializeArgumentsNames: aDRCode with: argumentsValues.

	self initializeScope.

	"Copy values to argument temps"
	argumentNames := aDRCode codeNode argumentNames.
	argumentNames withIndexDo: [ :name :index |
		| store |
		store := self
			         addInstructionWithNoResultFrom: self scope node
			         instructionKind: DRStoreTemporaryVariable
			         operands: {
					         name.
					         (argumentsValues at: index) }.
		store scope: self scope.
		self popOperand ]
]

{ #category : 'testing' }
DRMethodIRGeneratorInline >> isBaseMethodArgument: aRBVariableNode [
	"Here we check if the variable is accessing to a temporary (mainly arguments) from the compiled method.
	This change between accessing a variable from the method to inline and from the method where it is inlined."

	| variableDefinitionScope |
	variableDefinitionScope := aRBVariableNode scope.
	[ variableDefinitionScope hasTempDefinitionFor: aRBVariableNode name ]
		whileFalse: [
		variableDefinitionScope := variableDefinitionScope outerScope ].

	^ executionState baseFrame method = variableDefinitionScope node
]

{ #category : 'building' }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> outerScope [

	^ outerScope
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> outerScope: aDRScope [

	outerScope := aDRScope
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> scope [

	^ scope
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> setupCFGScope: aCodeNode [

	| temporaries arguments |
	self assert: self scope isEmpty.

	temporaries := aCodeNode temporaries collect: [ :t | t variable originalVar ].
	arguments := aCodeNode arguments collect: [ :a | a variable ].

	self scope addTempVariables: temporaries , arguments.
	self scope node: aCodeNode
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> visitReturnNode: aRBReturnNode [

	| isLocal returnValue returnScope |
	isLocal := executionState baseFrame method = aRBReturnNode methodNode.
	isLocal ifTrue: [ ^ super visitReturnNode: aRBReturnNode ].

	returnValue := self visitOperand: aRBReturnNode value.
	returnScope := self scope lookupScopeForNode:
		               aRBReturnNode methodNode.

	"Perform all the IR manipulation to add the new value as return in correct place"
	returnScope returnInstruction addNewReturn: returnValue.
	self newBasicBlock. "This basic block finish here"

	self pushOperand: returnValue
]
