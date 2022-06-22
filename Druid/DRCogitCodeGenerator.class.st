Class {
	#name : #DRCogitCodeGenerator,
	#superclass : #Object,
	#instVars : [
		'generatorMethodBuilder',
		'fallthroughBlock',
		'deferredBranches',
		'nextJumpIndex',
		'visitedBlocks'
	],
	#category : #'Druid-Cogit'
}

{ #category : #'ir-generation' }
DRCogitCodeGenerator >> buildStatementsForInstruction: anInstruction [ 

	^ anInstruction acceptVisitor: self
]

{ #category : #'jump-management' }
DRCogitCodeGenerator >> deferBranch: aString to: aDRBasicBlock [ 
	
	(deferredBranches at: aDRBasicBlock ifAbsentPut: [ OrderedCollection new ])
		add: aString
]

{ #category : #'as yet unclassified' }
DRCogitCodeGenerator >> generateCodeForCFG: aDRControlFlowGraph [

	| blocks |
	self generatePreambleForCFG: aDRControlFlowGraph.
	
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
DRCogitCodeGenerator >> generateJump: jumpKind target: aTarget [

	| jumpStatement jumpVariableName jumpTarget |
	"If the jump target is the next block in the order, do not add any jump to it"
	aTarget = fallthroughBlock ifTrue: [ ^ self ].

	jumpVariableName := 'jump', self nextJumpIndex asString.
	generatorMethodBuilder addVariableNamed: jumpVariableName.
	
	(self hasVisitedBlock: aTarget) ifTrue: [ 
		"Backwards branch, the jump target already exists with a label.
		Make the jump point to the already generated target"
		jumpTarget := self labelForBlock: aTarget
	] ifFalse: [
		"Forward branch, the jump target does not yet exist.
		Put a 0 for now to generate `cogit Jump: 0`.
		Defer the branch so it is resolved when the target is created"
		self deferBranch: jumpVariableName to: aTarget.
		jumpTarget := 0 asLiteralNode ].
	
	jumpStatement := RBAssignmentNode
		variable: (RBVariableNode named: jumpVariableName)
		value: (RBMessageNode
			receiver: RBVariableNode selfNode
			selector: jumpKind
			arguments: { jumpTarget }).
	
	generatorMethodBuilder addStatement: jumpStatement.
	
]

{ #category : #'as yet unclassified' }
DRCogitCodeGenerator >> generatePreambleForCFG: aDRControlFlowGraph [

	| addStatement |
	generatorMethodBuilder addVariableNamed: 'currentBlock'.
	
	"If we need no spill slots, do nothing"
	aDRControlFlowGraph numberOfSpillSlots = 0 ifTrue: [ ^ self ].
	
	"Otherwise, increment the stack pointer by N*WordSize where N is the number of required spill slots.
	The first spill slot will be [SP], the second will be [SP+WordSize] and so on."
	addStatement := RBMessageNode
		receiver: RBVariableNode selfNode
		selector: #AddCq:R:
		arguments: {
			RBLiteralValueNode value: aDRControlFlowGraph numberOfSpillSlots * 8 "bytes".
			RBVariableNode named: 'SPReg'
		}.
	generatorMethodBuilder addStatement: addStatement.
]

{ #category : #'ir-to-ast' }
DRCogitCodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph primitiveName: aName [

	generatorMethodBuilder selector: aName.
	self generateCodeForCFG: aDRControlFlowGraph.
	^ generatorMethodBuilder methodNode
]

{ #category : #accessing }
DRCogitCodeGenerator >> generatorMethodBuilder [
	^ generatorMethodBuilder
]

{ #category : #testing }
DRCogitCodeGenerator >> hasVisitedBlock: aDRBasicBlock [ 
	
	^ visitedBlocks includesKey: aDRBasicBlock
]

{ #category : #initialization }
DRCogitCodeGenerator >> initialize [

	super initialize.
	generatorMethodBuilder := DRGeneratorMethodBuilder new.
	deferredBranches := Dictionary new.
	nextJumpIndex := 0.

	visitedBlocks := Dictionary new
]

{ #category : #accessing }
DRCogitCodeGenerator >> labelForBlock: aDRBasicBlock [ 
	
	^ RBVariableNode named: (visitedBlocks at: aDRBasicBlock)
]

{ #category : #'jump-management' }
DRCogitCodeGenerator >> nextJumpIndex [
	
	| allocatedJumpIndex |
	allocatedJumpIndex := nextJumpIndex.
	nextJumpIndex := nextJumpIndex + 1.
	^ allocatedJumpIndex
]

{ #category : #'ir-to-ast' }
DRCogitCodeGenerator >> preProcessBlock: aDRBasicBlock [
	
	| branches |
	aDRBasicBlock hasIncomingBackjumps ifTrue: [ | blockIdentifier |
		"If this block has incoming backjumps, we need to put a label for it and store it"
		blockIdentifier := 'b', aDRBasicBlock id asString.
		generatorMethodBuilder addVariableNamed: blockIdentifier.
		generatorMethodBuilder addStatement: (RBAssignmentNode
			variable: (RBVariableNode named: blockIdentifier)
			value: (RBMessageNode
				receiver: RBVariableNode selfNode
				selector: #Label)).
		visitedBlocks at: aDRBasicBlock put: blockIdentifier ].

	"If someother block is forward jumping to this block, create a label and wire deferred jumps to it"
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
DRCogitCodeGenerator >> registerFor: aValue [

	^ aValue result name
]

{ #category : #visiting }
DRCogitCodeGenerator >> rtlArgumentsForValue: aValue [

	"If it is a spilling stack location, we need to return a pair (offset, baseRegister)"
	aValue isMemoryAddress ifTrue: [ ^ aValue rtlArgumentExpressions ].

	"Otherwise just return the expression for the value"
	^ { (self rtlExpressionForValue: aValue) }
]

{ #category : #visiting }
DRCogitCodeGenerator >> rtlExpressionForValue: aValue [

	| constantValue |
	aValue isConstant ifTrue: [ 
		"Interpret the booleans as the bits 0 and 1"
		constantValue := (#( true false ) includes: aValue value)
			ifTrue: [ aValue value asBit ]
			ifFalse: [ aValue value ].
		
		"Interpret nil as zero"
		constantValue isNil ifTrue: [ constantValue := 0 ].
		
		(constantValue isInteger and: [ constantValue > 5000 ]) ifTrue: [ 
			^ RBLiteralValueNode new
				  value: constantValue
				  start: 0
				  stop: -1
				  source: constantValue hex ].
		^ RBLiteralValueNode value: constantValue ].

	^ RBVariableNode named: (self registerFor: aValue)
]

{ #category : #visiting }
DRCogitCodeGenerator >> threeOperandCogitRTL: aMnemonic instruction: instruction [

	"Verify that the 3 address code instruction is actually in 2AC after register allocation"

	| allocatedOperands |
	allocatedOperands := (instruction operands collect: [ :op | 
		                      op result ]) asSet.
	instruction result isNoResult ifFalse: [ 
		allocatedOperands add: instruction result ].
	allocatedOperands size > 2 ifTrue: [ 
		self error: 'Cannot write instruction with more than 2 operands: '
			, instruction asString ].

	^ self
		  twoOperandCogitRTL: aMnemonic
		  operands: { 
				  instruction operand2.
				  instruction operand1 }
		  instruction: instruction
]

{ #category : #visiting }
DRCogitCodeGenerator >> twoOperandCogitRTL: aMnemonic operands: operands instruction: anInstruction [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector:
				 aMnemonic
				 , (operands first result rtlOperandQualifierForInstruction:
						  anInstruction)
				 , (operands second result rtlOperandQualifierForInstruction:
						  anInstruction)
			 arguments: (self rtlArgumentsForValue: operands first)
				 , (self rtlArgumentsForValue: operands second))
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitAddition: aDRCogitAdd [ 

	^ self threeOperandCogitRTL: #Add instruction: aDRCogitAdd 
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitAnd: aDRAnd [

	^ self threeOperandCogitRTL: #And instruction: aDRAnd
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitBitAnd: aDRCogitBitAnd [ 

	^ self threeOperandCogitRTL: #And instruction: aDRCogitBitAnd

]

{ #category : #visiting }
DRCogitCodeGenerator >> visitBitOr: aDRCogitBitAnd [ 

	^ self threeOperandCogitRTL: #Or instruction: aDRCogitBitAnd

]

{ #category : #visiting }
DRCogitCodeGenerator >> visitBitShift: aDRCogitBitShift [ 

	^ self threeOperandCogitRTL: #LogicalShiftLeft instruction: aDRCogitBitShift

]

{ #category : #visiting }
DRCogitCodeGenerator >> visitComparison: aComparison [

	| comparands |
	"Generate comparison instruction. In cogit, if you want to do A < B, you have to generate:
	CMP B A
	JumpLessThan xxx
	Thus, operands are inverted always"
	comparands := aComparison operands reversed.

	"Cogit RTL only supports constants as the first argument.
	Invert the comparison and the condition if necessary, mostly for greaterThan comparisons)."
	comparands second isConstant ifTrue: [ 
		aComparison replaceBy: aComparison reversed.
		comparands := comparands reversed ].

	self
		twoOperandCogitRTL: #Cmp
		operands: comparands
		instruction: aComparison
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitConditionalJump: aDRConditionalJump [ 

	"Generate both jumps, the fallthorugh will get ignored"
	self generateJump: aDRConditionalJump condition cogitTrueConditionCode target: aDRConditionalJump trueBranch.
	self generateJump: aDRConditionalJump condition cogitFalseConditionCode target: aDRConditionalJump falseBranch
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitCopy: aDRMove [

	"Do not generate moves of null values"
	| moveOperands |
	aDRMove operands first isNullValue
		ifTrue: [ ^ self ].
	
	moveOperands := { aDRMove operand1. aDRMove result }.
	
	"Do not generate redundant moves (e.g., r0 := r0)"
	(moveOperands collect: [ :e | e result ] as: Set) size = 1
		ifTrue: [ ^ self ].
	
	self twoOperandCogitRTL: #Move operands: moveOperands instruction: aDRMove
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitDivision: aDRCogitDivision [

	"Cogit division is not in 2AC but in 4AC.
	The (cogit specific) register allocator should inserts a register for REM destination implicitly yet unused.
	It should also put all operands in registers as the cogit instruction does not support constants."

	^ generatorMethodBuilder addStatement: (RBMessageNode
			   receiver: RBVariableNode selfNode
			   selector: #DivR:R:Quo:Rem:
			   arguments: { 
					   (self rtlExpressionForValue: aDRCogitDivision operands second).
					   (self rtlExpressionForValue: aDRCogitDivision operands first).
					   (self rtlExpressionForValue: aDRCogitDivision result).
					   (self rtlExpressionForValue: aDRCogitDivision operands third) })
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitEqualsComparison: aDRCogitEqualsComparison [ 
	
	self visitComparison: aDRCogitEqualsComparison
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitGreaterOrEqualsThanComparison: aDRGreaterOrEqualsThanComparison [ 
	
	self visitComparison: aDRGreaterOrEqualsThanComparison
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitGreaterThanComparison: aDRGreaterThanComparison [ 

	self visitComparison: aDRGreaterThanComparison
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitJump: aDRJump [ 

	self
		generateJump: #Jump:
		target: aDRJump target
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitLeftShift: aDRCogitLeftShift [
	^ self visitBitShift: (DRBitShift operands: aDRCogitLeftShift operands result:  aDRCogitLeftShift result)
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitLessOrEqualsThanComparison: aDRCogitLessOrEqualsComparison [ 
	
	self visitComparison: aDRCogitLessOrEqualsComparison
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitLessThanComparison: aDRLessThanComparison [ 
	
	self visitComparison: aDRLessThanComparison
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitLoad: aDRLoad [

	"If it is smaller than the word size, zero the target register before"

	aDRLoad size < 8 ifTrue: [ 
		^ self
			twoOperandCogitRTL: #Move
			operands: { 
					0 asDRValue.
					aDRLoad result }
			instruction: aDRLoad ].

	^ self
		  twoOperandCogitRTL: #Move
		  operands: { 
				  aDRLoad operand1 asMemoryAddress.
				  aDRLoad result }
		  instruction: aDRLoad
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitMod: aDRCogitDivision [

	"Cogit division (and thus remainder too) is not in 2AC but in 4AC.
	The (cogit specific) register allocator should inserts a register for quotient destination implicitly yet unused.
	It should also put all operands in registers as the cogit instruction does not support constants."

	^ generatorMethodBuilder addStatement: (RBMessageNode
			   receiver: RBVariableNode selfNode
			   selector: #DivR:R:Quo:Rem:
			   arguments: { 
					   (self rtlExpressionForValue: aDRCogitDivision operands second).
					   (self rtlExpressionForValue: aDRCogitDivision operands first).
					   (self rtlExpressionForValue: aDRCogitDivision operands third).
					   (self rtlExpressionForValue: aDRCogitDivision result) })
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitMultiply: aDRCogitMultiply [ 

	^ self threeOperandCogitRTL: #Mul instruction: aDRCogitMultiply

]

{ #category : #visiting }
DRCogitCodeGenerator >> visitMultiplyOverflowJump: aDROverflowJump [ 
	
	self generateJump: #JumpMultiplyOverflow: target: aDROverflowJump trueBranch.
	self generateJump: #JumpMultiplyNoOverflow: target: aDROverflowJump falseBranch.
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitNegate: aDRCogitNegate [ 
	
	self twoOperandCogitRTL: #Move
		operands: { aDRCogitNegate operand1. aDRCogitNegate result }
		instruction: aDRCogitNegate.
	self flag: #todo. "oneOperandCogitRTL:"
	generatorMethodBuilder addStatement: (
		RBMessageNode
			receiver: RBVariableNode selfNode
			selector: #NegateR:
			arguments: { 
				RBVariableNode named: (self registerFor: aDRCogitNegate result) })
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitNoop: aDRNoop [ 
	
	"Do nothing"
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitNotEqualsComparison: aDRCogitNotEqualsComparison [ 
	
	self visitComparison: aDRCogitNotEqualsComparison
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitNullValue: aDRNullValue [ 

	^ RBMessageNode
		receiver: RBVariableNode selfNode
		selector: #Nop
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitOr: aDROr [ 

	^ self threeOperandCogitRTL: #Or instruction: aDROr
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitOverflowJump: aDROverflowJump [ 
	
	"Generate both jumps, the fallthorugh will get ignored"
	self generateJump: #JumpOverflow: target: aDROverflowJump trueBranch.
	self generateJump: #JumpNoOverflow: target: aDROverflowJump falseBranch.
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitPhiFunction: aDRPhiFunction [ 

	"For the moment nothing"
	^ self visitNullValue: aDRPhiFunction
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitReturn: aDRReturn [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genPrimReturn)
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitRightShift: aDRCogitRightShift [ 

	^ self threeOperandCogitRTL: #LogicalShiftRight instruction: aDRCogitRightShift

]

{ #category : #visiting }
DRCogitCodeGenerator >> visitStore: aDRCogitStore [

	^ self
		  twoOperandCogitRTL: #Move
		  operands: { 
				  aDRCogitStore operand1.
				  aDRCogitStore operand2 asMemoryAddress }
		  instruction: aDRCogitStore
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitSubstract: aDRCogitSubstract [ 
	
	^ self threeOperandCogitRTL: #Sub instruction: aDRCogitSubstract

]

{ #category : #visiting }
DRCogitCodeGenerator >> visitTest: aTest [

	"Cogit RTL only supports constants as the first argument.
	Invert them  (and the condition if necessary, mostly for greaterThan comparisons)."

	| comparands |
	comparands := aTest operands.
	aTest operands second isConstant ifTrue: [ 
		aTest replaceBy: aTest reversed.
		comparands := comparands reversed ].
	self twoOperandCogitRTL: #Tst operands: comparands instruction: aTest
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitUnsignedCoercion: aDRCogitUnsignedCoercion [ 
	
	"Nothing for now"
	^ self visitNullValue: aDRCogitUnsignedCoercion
]
