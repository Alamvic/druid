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
DRMethodIRGeneratorInline >> allTemporaryInstructions [

	^ executionState allTemporaryInstructions
]

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

	scope := DRScope new
		         outerScope: self outerScope;
		         id: self outerScope id + 1;
		         yourself.

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

	"Remove this if by a better support for inlined blocks"
	aDRCode isDRBlockClosure ifFalse: [
		self currentBasicBlock initializeScope: self scope ].

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

	self scope addTemps: aCodeNode temporaryNames, aCodeNode argumentNames
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> visitArgumentVariableNode: aRBVariableNode [ 
	
	(self isBaseMethodArgument: aRBVariableNode)
		ifTrue: [
			super visitArgumentVariableNode: aRBVariableNode .]
		ifFalse: [
			| value |
			"If it is not accesssing a method argument, 
			then get the target instruction and push it"
			value := self temporaryAt: aRBVariableNode name.
			^ self pushOperand: value ]
]
