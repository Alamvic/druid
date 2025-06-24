Class {
	#name : 'DRMethodIRGeneratorInline',
	#superclass : 'DRMethodIRGenerator',
	#category : 'Druid-CompilerCompiler',
	#package : 'Druid',
	#tag : 'CompilerCompiler'
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
	[ executionState executionStack size = 0 ] whileFalse: [ self popFrame ].
	currentBasicBlock := lastBlock
]

{ #category : 'accessing' }
DRMethodIRGeneratorInline >> handleCFGScope: methodNode [

	"here to handle the scope with inlining"
]

{ #category : 'testing' }
DRMethodIRGeneratorInline >> isBaseMethodArgument: aRBVariableNode [

	"Here we check if the variable is accessing to a temporary (mainly arguments) from the compiled method.
	This change between accessing a variable from the method to inline and from the method where it is inlined."
	
	| variableScope |
	variableScope := aRBVariableNode variable scope.
	[variableScope isMethodScope] whileFalse: [ 
		variableScope := variableScope outerScope ].

	^ executionState baseFrame method = variableScope node
]

{ #category : 'building' }
DRMethodIRGeneratorInline >> newFirstBasicBlock [

	^ self newBasicBlock
]

{ #category : 'visiting' }
DRMethodIRGeneratorInline >> setupCFGScope: aNode [

	| scope |
	scope := self ir scope.
	scope addTemps: aNode temporaryNames.

	"aNode methodNode == executionState baseFrame method ifTrue: [
		scope addArguments: aNode argumentNames ]"
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
