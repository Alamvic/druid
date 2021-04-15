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
		'targetAST'
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
	registerAllocator allocateRegistersIn: druidIR.
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

{ #category : #'ir-to-ast' }
DRCompilerCompiler >> preProcessBlock: aDRBasicBlock [ 
	
	| branches |
	generatorMethodBuilder addStatement: (RBAssignmentNode
		variable: (RBVariableNode named: 'currentBlock')
		value: (RBMessageNode
			receiver: RBVariableNode selfNode
			selector: #Label)).
	
	branches := deferredBranches at: aDRBasicBlock ifAbsent: [ ^ self ].
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
					self haltIf: [ value result isKindOf: DRNoRegister ].
					copy := DRCopy operands: { value result } result: i result.
					originBlock ifNotNil: [originBlock addInstruction: copy].
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
DRCompilerCompiler >> visitConditionalJump: aDRConditionalJump [ 

	"Fall-through true branch, jump to false branch"
	| simplifiedCondition |
	simplifiedCondition := aDRConditionalJump condition singleChild.

	self jumpForwardCogRTL: simplifiedCondition cogitFalseConditionCode instruction: aDRConditionalJump
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
DRCompilerCompiler >> visitOr: aDROr [ 

	^ self twoOperandCogitRTL: #Or instruction: aDROr
]

{ #category : #visiting }
DRCompilerCompiler >> visitOverflowJump: aDROverflowJump [ 
	
	"Fall-through true branch, jump to false branch"
	^ self jumpForwardCogRTL: #JumpNoOverflow: instruction: aDROverflowJump
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
