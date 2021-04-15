Class {
	#name : #DRPrimitiveCompilerCompiler,
	#superclass : #DRCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
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
	
	generatorMethodBuilder addVariableNamed: 'currentBlock'.
	aDRControlFlowGraph blocksDo: [ :b |
		self preProcessBlock: b.
		b instructionsDo: [ :e | 
			self buildStatementsForInstruction: e ] ].
	generatorMethodBuilder addStatement: (RBReturnNode value: (RBVariableNode named: 'CompletePrimitive'))
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> generateJump: jumpKind target: aTarget [

	| jumpStatement jumpVariableName |
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
				RBVariableNode named: (self registerFor: (operands at: 1)).
				RBVariableNode named: (self registerFor: (operands at: 2)). })
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitAddition: aDRCogitAdd [ 

	self twoOperandCogitRTL: #Move operands: {
		aDRCogitAdd operand1. aDRCogitAdd result }.
	^ self twoOperandCogitRTL: #Add operands: { 
		aDRCogitAdd operand2. aDRCogitAdd result }
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitBitAnd: aDRCogitBitAnd [ 

	^ self twoOperandCogitRTL: #And instruction: aDRCogitBitAnd
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitBitShift: aDRCogitBitShift [ 

	self twoOperandCogitRTL: #Move operands: {
		aDRCogitBitShift operand1. aDRCogitBitShift result }.
	^ self twoOperandCogitRTL: #LogicalShiftLeft operands: { 
		aDRCogitBitShift operand2. aDRCogitBitShift result }
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitEqualsComparison: aDRCogitEqualsComparison [ 
	
	"Cogit RTL only supports constants as the first argument.
	Invert them  (and the condition if necessary, mostly for greaterThan comparisons)."
	| comparands condition |
	comparands := aDRCogitEqualsComparison operands.
	aDRCogitEqualsComparison operands second isConstant
		ifTrue: [ 
			comparands := comparands reversed ].
	
	self twoOperandCogitRTL: #Cmp operands: comparands.
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitJump: aDRJump [ 

	self
		generateJump: #Jump:
		target: aDRJump target
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitLeftShift: aDRCogitLeftShift [ 

	^ self twoOperandCogitRTL: #LogicalShiftLeft instruction: aDRCogitLeftShift
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitLessOrEqualsComparison: aDRCogitLessOrEqualsComparison [ 
	
	self twoOperandCogitRTL: #Cmp operands: aDRCogitLessOrEqualsComparison operands
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitLoad: aDRCogitLoad [ 
	
	^ self twoOperandCogitRTL: #Move instruction: aDRCogitLoad
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitMultiply: aDRCogitMultiply [ 
	
	^ self twoOperandCogitRTL: #Mul instruction: aDRCogitMultiply
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitNegate: aDRCogitNegate [ 
	
	self twoOperandCogitRTL: #Move instruction: aDRCogitNegate.
	generatorMethodBuilder addStatement: (
		RBMessageNode
			receiver: RBVariableNode selfNode
			selector: #NegateR:
			arguments: { 
				RBVariableNode named: (self registerFor: aDRCogitNegate result) })
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitNullValue: aDRNullValue [ 

	^ RBMessageNode
		receiver: RBVariableNode selfNode
		selector: #Nop
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitPhiFunction: aDRPhiFunction [ 

	"For the moment nothing"
	^ self visitNullValue: aDRPhiFunction
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitRightShift: aDRCogitRightShift [ 

	^ self twoOperandCogitRTL: #LogicalShiftRight instruction: aDRCogitRightShift
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitStore: aDRCogitStore [

	^ self twoOperandCogitRTL: #Move instruction: aDRCogitStore
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitSubstract: aDRCogitSubstract [ 
	
	self twoOperandCogitRTL: #Move operands: {
		aDRCogitSubstract operand1. aDRCogitSubstract result }.
	^ self twoOperandCogitRTL: #Sub operands: { 
		aDRCogitSubstract operand2. aDRCogitSubstract result }
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitTest: aDRCogitTest [
	| firstTestOperand |
	firstTestOperand := aDRCogitTest operand2 singleChild operands first.
	firstTestOperand isConstant
		ifFalse: [ self error: 'Cannot compile Tst instruction with non constant operand' ].
	^ self twoOperandCogitRTL: #Tst operands: {
		firstTestOperand. aDRCogitTest operand1 }
]

{ #category : #visiting }
DRPrimitiveCompilerCompiler >> visitUnsignedCoercion: aDRCogitUnsignedCoercion [ 
	
	"Nothing for now"
	^ self visitNullValue: aDRCogitUnsignedCoercion
]

{ #category : #'ast-to-source' }
DRPrimitiveCompilerCompiler >> writeAST: aRBMethodNode [ 
	
	compilerClass compile: aRBMethodNode formattedCode classified: #generated
]
