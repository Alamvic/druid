Class {
	#name : #DRCompilerCompiler,
	#superclass : #Object,
	#instVars : [
		'interpreter',
		'compilerClass',
		'sourceName',
		'allocatedRegisters',
		'generatorMethodBuilder',
		'nextJumpIndex',
		'deferredBranches',
		'registerAllocator',
		'druidIR',
		'targetAST',
		'optimisations'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'ir-generation' }
DRCompilerCompiler >> buildStatementsForInstruction: anInstruction [ 

	^ anInstruction acceptVisitor: self
]

{ #category : #api }
DRCompilerCompiler >> compile [

	druidIR := self generateDruidIR.
1halt.
	self optimize: druidIR.

	registerAllocator allocateRegistersIn: druidIR.
	
	"Remove the phi functions.
	From this point on, we are not in SSA form anymore"
	self resolvePhiFunctionsIn: druidIR.
	
	targetAST := self generateTargetASTFromIR: druidIR.
	self writeAST: targetAST
]

{ #category : #accessing }
DRCompilerCompiler >> compilerClass [

	^ compilerClass
]

{ #category : #accessing }
DRCompilerCompiler >> compilerClass: anObject [

	compilerClass := anObject
]

{ #category : #'jump-management' }
DRCompilerCompiler >> deferBranch: aString to: aDRBasicBlock [ 
	
	(deferredBranches at: aDRBasicBlock ifAbsentPut: [ OrderedCollection new ])
		add: aString
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> generateDruidIR [
	
	"Generates and returns an intermediate representation of the primitive from the interpreter primitive"
	| method irGenerator |
	method := interpreter class lookupSelector: sourceName.
	irGenerator := self newIRGenerator.
	irGenerator
		newBasicBlock;
		numberOfArguments: (method ast pragmaNamed: #numberOfArguments:) arguments first value;
		interpretMethod: method ast
		receiver: (DRInterpreterValue value: interpreter).
	^ irGenerator ir
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> initialize [

	super initialize.
	nextJumpIndex := 0.
	allocatedRegisters := Dictionary new.
	deferredBranches := Dictionary new.
	generatorMethodBuilder := DRGeneratorMethodBuilder new.
	registerAllocator := DRLinearScanRegisterAllocator new.
	
	optimisations := OrderedCollection new.
	optimisations add: DRSCCP new.
]

{ #category : #accessing }
DRCompilerCompiler >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRCompilerCompiler >> interpreter: anObject [

	interpreter := anObject
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> newIRGenerator [

	^ DRIRGenerator new
]

{ #category : #'jump-management' }
DRCompilerCompiler >> nextJumpIndex [
	
	| allocatedJumpIndex |
	allocatedJumpIndex := nextJumpIndex.
	nextJumpIndex := nextJumpIndex + 1.
	^ allocatedJumpIndex
]

{ #category : #api }
DRCompilerCompiler >> optimize: anIR [

	optimisations do: [ :optimization |
		optimization applyTo: anIR ]
]

{ #category : #'ir-to-ast' }
DRCompilerCompiler >> preProcessBlock: aDRBasicBlock [
	
	| branches |
	branches := deferredBranches at: aDRBasicBlock ifAbsent: [ ^ self ].
	
	generatorMethodBuilder addStatement: (RBAssignmentNode
		variable: (RBVariableNode named: 'currentBlock')
		value: (RBMessageNode
			receiver: RBVariableNode selfNode
			selector: #Label)).

	branches do: [ :e |
		generatorMethodBuilder addStatement: (RBMessageNode
			receiver: (RBVariableNode named: e)
			selector: #jmpTarget:
			arguments: { RBVariableNode named: 'currentBlock' }). ]
]

{ #category : #registers }
DRCompilerCompiler >> registerFor: aValue [
	
	aValue isConstant ifTrue: [ 
		(aValue value isInteger and: [ aValue value > 5000 ])
			ifTrue: [ ^ aValue value hex ].
		^ aValue value asString ].

	^ aValue result name
]

{ #category : #compilation }
DRCompilerCompiler >> resolvePhiFunctionsIn: aDRControlFlowGraph [

	"Create a copy instruction for each of the values of the Phi function.
	Put the copy instruction at the end of the block generating the value.
	
	For example
	for t3 := phi(t1,t2)
		with t1 coming from block A and t2 coming from block B.
	create a copy t3 := t1 at the end of block A
	create a copy t3 := t2 at the end of block B"
	
	aDRControlFlowGraph instructionsDo: [ :i |
		i isPhiFunction ifTrue: [ 
			i valuesWithOriginDo: [ :value :originBlock | | copy |
				(value isKindOf: DRNullValue) ifFalse: [ 
					(value result isKindOf: DRNoRegister)
						ifFalse: [ 
							copy := DRCopy operands: { value result } result: i result.
							originBlock ifNotNil: [originBlock addInstruction: copy] ].
			] ] ] ]
]

{ #category : #accessing }
DRCompilerCompiler >> sourceName [

	^ sourceName
]

{ #category : #accessing }
DRCompilerCompiler >> sourceName: anObject [

	sourceName := anObject
]

{ #category : #visiting }
DRCompilerCompiler >> visitAddition: aDRCogitAdd [ 

	self twoOperandCogitRTL: #Move operands: {
		aDRCogitAdd operand1. aDRCogitAdd result }.
	^ self twoOperandCogitRTL: #Add operands: { 
		aDRCogitAdd operand2. aDRCogitAdd result }
]

{ #category : #visiting }
DRCompilerCompiler >> visitBitAnd: aDRCogitBitAnd [ 

	^ self twoOperandCogitRTL: #And instruction: aDRCogitBitAnd
]

{ #category : #visiting }
DRCompilerCompiler >> visitBitShift: aDRCogitBitShift [ 

	self twoOperandCogitRTL: #Move operands: {
		aDRCogitBitShift operand1. aDRCogitBitShift result }.
	^ self twoOperandCogitRTL: #LogicalShiftLeft operands: { 
		aDRCogitBitShift operand2. aDRCogitBitShift result }
]

{ #category : #visiting }
DRCompilerCompiler >> visitComparison: aComparison [ 
	
	"Cogit RTL only supports constants as the first argument.
	Invert them  (and the condition if necessary, mostly for greaterThan comparisons)."
	| comparands |
	comparands := aComparison operands.
	aComparison operands second isConstant
		ifTrue: [
			aComparison replaceBy: aComparison reversed.
			comparands := comparands reversed ].
	self twoOperandCogitRTL: #Cmp operands: comparands
]

{ #category : #visiting }
DRCompilerCompiler >> visitConditionalJump: aDRConditionalJump [ 

	"Fall-through true branch, jump to false branch"
	self jumpForwardCogRTL: aDRConditionalJump condition singleChild cogitTrueConditionCode instruction: aDRConditionalJump
]

{ #category : #visiting }
DRCompilerCompiler >> visitConstantCondition: aDRConstantCondition [
	
	"If true, jump to the true branch.
	If false, jump to the false branch."
	
	"Code generated in the conditional jump"
]

{ #category : #visiting }
DRCompilerCompiler >> visitCopy: aDRMove [

	^ self twoOperandCogitRTL: #Move instruction: aDRMove
]

{ #category : #visiting }
DRCompilerCompiler >> visitEqualsComparison: aDRCogitEqualsComparison [ 
	
	self visitComparison: aDRCogitEqualsComparison
]

{ #category : #visiting }
DRCompilerCompiler >> visitGreaterOrEqualsThanComparison: aDRGreaterOrEqualsThanComparison [ 
	
	self visitComparison: aDRGreaterOrEqualsThanComparison
]

{ #category : #visiting }
DRCompilerCompiler >> visitGreaterThanComparison: aDRGreaterThanComparison [ 

	self visitComparison: aDRGreaterThanComparison
]

{ #category : #visiting }
DRCompilerCompiler >> visitJump: aDRJump [ 

	self
		generateJump: #Jump:
		target: aDRJump target
]

{ #category : #visiting }
DRCompilerCompiler >> visitLeftShift: aDRCogitLeftShift [ 

	^ self twoOperandCogitRTL: #LogicalShiftLeft instruction: aDRCogitLeftShift
]

{ #category : #visiting }
DRCompilerCompiler >> visitLessOrEqualsThanComparison: aDRCogitLessOrEqualsComparison [ 
	
	self visitComparison: aDRCogitLessOrEqualsComparison
]

{ #category : #visiting }
DRCompilerCompiler >> visitLessThanComparison: aDRLessThanComparison [ 
	
	self visitComparison: aDRLessThanComparison
]

{ #category : #visiting }
DRCompilerCompiler >> visitLoad: aDRCogitLoad [ 
	
	^ self twoOperandCogitRTL: #Move instruction: aDRCogitLoad
]

{ #category : #visiting }
DRCompilerCompiler >> visitMultiply: aDRCogitMultiply [ 
	
	^ self twoOperandCogitRTL: #Mul instruction: aDRCogitMultiply
]

{ #category : #visiting }
DRCompilerCompiler >> visitNegate: aDRCogitNegate [ 
	
	self twoOperandCogitRTL: #Move instruction: aDRCogitNegate.
	generatorMethodBuilder addStatement: (
		RBMessageNode
			receiver: RBVariableNode selfNode
			selector: #NegateR:
			arguments: { 
				RBVariableNode named: (self registerFor: aDRCogitNegate result) })
]

{ #category : #visiting }
DRCompilerCompiler >> visitNoop: aDRNoop [ 
	
	"Do nothing"
]

{ #category : #visiting }
DRCompilerCompiler >> visitNotEqualsComparison: aDRCogitNotEqualsComparison [ 
	
	self visitComparison: aDRCogitNotEqualsComparison
]

{ #category : #visiting }
DRCompilerCompiler >> visitNullValue: aDRNullValue [ 

	^ RBMessageNode
		receiver: RBVariableNode selfNode
		selector: #Nop
]

{ #category : #visiting }
DRCompilerCompiler >> visitOr: aDROr [ 

	^ self twoOperandCogitRTL: #Or instruction: aDROr
]

{ #category : #visiting }
DRCompilerCompiler >> visitOverflowJump: aDROverflowJump [ 
	
	"Fall-through true branch, jump to false branch"
	^ self jumpForwardCogRTL: #JumpNoOverflow: instruction: aDROverflowJump
]

{ #category : #visiting }
DRCompilerCompiler >> visitPhiFunction: aDRPhiFunction [ 

	"For the moment nothing"
	^ self visitNullValue: aDRPhiFunction
]

{ #category : #visiting }
DRCompilerCompiler >> visitReturn: aDRReturn [ 
	
	generatorMethodBuilder addStatement: (
		RBMessageNode
			receiver: RBVariableNode selfNode
			selector: #RetN:
			arguments: {
				RBMessageNode
					receiver: (RBVariableNode named: (self registerFor: aDRReturn operand1))
					selector: #*
					arguments: { RBMessageNode 
						receiver: (RBVariableNode named: 'objectMemory')
						selector: #wordSize } } )
]

{ #category : #visiting }
DRCompilerCompiler >> visitRightShift: aDRCogitRightShift [ 

	^ self twoOperandCogitRTL: #LogicalShiftRight instruction: aDRCogitRightShift
]

{ #category : #visiting }
DRCompilerCompiler >> visitStore: aDRCogitStore [

	^ self twoOperandCogitRTL: #Move instruction: aDRCogitStore
]

{ #category : #visiting }
DRCompilerCompiler >> visitSubstract: aDRCogitSubstract [ 
	
	self twoOperandCogitRTL: #Move operands: {
		aDRCogitSubstract operand1. aDRCogitSubstract result }.
	^ self twoOperandCogitRTL: #Sub operands: { 
		aDRCogitSubstract operand2. aDRCogitSubstract result }
]

{ #category : #visiting }
DRCompilerCompiler >> visitTest: aTest [
	
	"Cogit RTL only supports constants as the first argument.
	Invert them  (and the condition if necessary, mostly for greaterThan comparisons)."
	| comparands |
	comparands := aTest operands.
	aTest operands second isConstant
		ifTrue: [
			aTest replaceBy: aTest reversed.
			comparands := comparands reversed ].
	self twoOperandCogitRTL: #Tst operands: comparands
]

{ #category : #visiting }
DRCompilerCompiler >> visitUnsignedCoercion: aDRCogitUnsignedCoercion [ 
	
	"Nothing for now"
	^ self visitNullValue: aDRCogitUnsignedCoercion
]

{ #category : #'ast-to-source' }
DRCompilerCompiler >> writeAST: aRBMethodNode [ 
	
	compilerClass compile: aRBMethodNode formattedCode classified: #generated
]
