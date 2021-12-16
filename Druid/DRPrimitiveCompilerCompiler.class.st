Class {
	#name : #DRPrimitiveCompilerCompiler,
	#superclass : #DRCompilerCompiler,
	#instVars : [
		'primitiveCompilerName',
		'fallthroughBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #API }
DRPrimitiveCompilerCompiler class >> compilePrimitive: primitiveName from: anInterpreter as: primitiveCompilerName in: aCompilerClass [ 
	
	^ self new
		primitiveName: primitiveName;
		interpreter: anInterpreter;
		primitiveCompilerName: primitiveCompilerName;
		compilerClass: aCompilerClass;
		compile
]

{ #category : #'ir-to-ast' }
DRPrimitiveCompilerCompiler >> buildStatementsForCFG: aDRControlFlowGraph [ 
	
	| blocks |
	generatorMethodBuilder addVariableNamed: 'currentBlock'.
	
	"Sort the blocks"
	blocks := aDRControlFlowGraph reversePostOrderBlocks.

	"Make sure the failure exit block is at the end, so it works as a fallthrough with the following bytecode"
	(blocks includes: aDRControlFlowGraph failureExitBlock) ifTrue: [
		blocks remove: aDRControlFlowGraph failureExitBlock.
		blocks addLast: aDRControlFlowGraph failureExitBlock ].
	
	blocks withIndexDo: [ :b :i |
		fallthroughBlock := blocks at: i + 1 ifAbsent: nil.
		self preProcessBlock: b.
		b instructionsDo: [ :e | 
			self buildStatementsForInstruction: e ] ].
	generatorMethodBuilder addStatement: (RBReturnNode value: (RBVariableNode named: 'CompletePrimitive'))
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> generateJump: jumpKind target: aTarget [

	| jumpStatement jumpVariableName |
	"If the jump target is the next block in the order, do not add any jump to it"
	aTarget = fallthroughBlock ifTrue: [ ^ self ].

	jumpVariableName := 'jump', self nextJumpIndex asString.
	generatorMethodBuilder addVariableNamed: jumpVariableName.
	
	jumpStatement := RBAssignmentNode
		variable: (RBVariableNode named: jumpVariableName)
		value: (RBMessageNode
			receiver: RBVariableNode selfNode
			selector: jumpKind
			arguments: { 0 asLiteralNode }).
	
	generatorMethodBuilder addStatement: jumpStatement.
	self deferBranch: jumpVariableName to: aTarget.
]

{ #category : #'ir-to-ast' }
DRPrimitiveCompilerCompiler >> generateTargetASTFromIR: aDRControlFlowGraph [ 

	generatorMethodBuilder selector: primitiveCompilerName.
	self buildStatementsForCFG: aDRControlFlowGraph.
	^ generatorMethodBuilder methodNode
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> initialize [

	super initialize.
	
	optimisations := OrderedCollection new.
	optimisations add: DRDeadBlockElimination new.
	optimisations add: DRCleanControlFlow new.
	
	"Basic set of optimizations twice for primitives"
	optimisations add: DRFailureCodeBasedTailDuplication new.
	optimisations add: DRSCCP new.
	optimisations add: DRCopyPropagation new.
	optimisations add: DRDeadCodeElimination new.
	optimisations add: DRCleanControlFlow new.
	
	optimisations add: DRFailureCodeBasedTailDuplication new.
	optimisations add: DRSCCP new.
	optimisations add: DRCopyPropagation new.
	optimisations add: DRDeadCodeElimination new.
	optimisations add: DRCleanControlFlow new.
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> jumpForwardCogRTL: jumpKind instruction: aConditionalInstruction [

	self
		generateJump: jumpKind
		target: aConditionalInstruction falseBranch
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> newIRGenerator [

	^ DRPrimitiveIRGenerator new
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveCompilerName [

	^ primitiveCompilerName
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveCompilerName: anObject [

	primitiveCompilerName := anObject
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveName [

	^ self sourceName
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveName: anObject [

	self sourceName: anObject
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> rtlExpressionForValue: aValue [
	
	(#( true false ) includes: aValue value) ifTrue: [
		^ RBMessageNode
			receiver: (RBVariableNode named: aValue value asString)
			selector: #asBit
	].
	^ RBVariableNode named: (self registerFor: aValue)
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> twoOperandCogitRTL: aMnemonic instruction: instruction [

	self
		twoOperandCogitRTL: aMnemonic
		operands: {
		instruction operand1.
		instruction result }.
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> twoOperandCogitRTL: aMnemonic operands: operands [

	generatorMethodBuilder addStatement: (
		RBMessageNode
			receiver: RBVariableNode selfNode
			selector: (aMnemonic, 
				operands first result rtlOperandQualifier,
				operands second result rtlOperandQualifier)
			arguments: { 
				self rtlExpressionForValue: (operands at: 1).
				self rtlExpressionForValue: (operands at: 2) })
]
