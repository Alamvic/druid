Class {
	#name : #DRCogitCodeGenerator,
	#superclass : #Object,
	#instVars : [
		'generatorMethodBuilder',
		'fallthroughBlock',
		'deferredBranches',
		'nextJumpIndex'
	],
	#category : #'Druid-CompilerCompiler'
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

{ #category : #'as yet unclassified' }
DRCogitCodeGenerator >> initialize [
	super initialize.
	generatorMethodBuilder := DRGeneratorMethodBuilder new.
	deferredBranches := Dictionary new.
	nextJumpIndex := 0.
]

{ #category : #visiting }
DRCogitCodeGenerator >> jumpForwardCogRTL: jumpKind instruction: aConditionalInstruction [

	self
		generateJump: jumpKind
		target: aConditionalInstruction falseBranch
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
	aValue isMemoryAddress ifTrue: [ 
		^ { 
			  (self rtlExpressionForValue: (aValue stackOffset * 8 "bytes") asDRValue).
			  (self rtlExpressionForValue: (DRPhysicalRegister name: 'SPReg')) } ].

	"Otherwise just return the expression for the value"
	^ { (self rtlExpressionForValue: aValue) }
]

{ #category : #visiting }
DRCogitCodeGenerator >> rtlExpressionForValue: aValue [

	(#( true false ) includes: aValue value) ifTrue: [ 
		^ RBMessageNode
			  receiver: (RBVariableNode named: aValue value asString)
			  selector: #asBit ].

	aValue isConstant ifTrue: [ 
		(aValue value isInteger and: [ aValue value > 5000 ]) ifTrue: [ 
			^ RBLiteralValueNode new
				  value: aValue value
				  start: 0
				  stop: -1
				  source: aValue value hex ].
		^ RBLiteralValueNode value: aValue value ].

	^ RBVariableNode named: (self registerFor: aValue)
]

{ #category : #visiting }
DRCogitCodeGenerator >> threeOperandCogitRTL: aMnemonic instruction: instruction [

	"Verify that the 3 address code instruction is actually in 2AC after register allocation"
	| allocatedOperands |
	allocatedOperands := ({instruction result }, (instruction operands collect: [:op | op result])) asSet.
	allocatedOperands size > 2 ifTrue: [ self error: 'Cannot write instruction with more than 2 operands: ', instruction asString ].
	
	^ self twoOperandCogitRTL: aMnemonic operands: { 
			  instruction operand2.
			  instruction result }
]

{ #category : #visiting }
DRCogitCodeGenerator >> twoOperandCogitRTL: aMnemonic instruction: instruction [

	| destination replacement |
	replacement := instruction operand1 isMemoryAddress
		               ifTrue: [ 
			               self twoOperandCogitRTL: 'Move' operands: { 
					               instruction operand1.
					               (DRPhysicalRegister name: 'TempReg') }.
			               DRPhysicalRegister name: 'TempReg' ]
		               ifFalse: [ instruction operand1 ].

	destination := instruction result isMemoryAddress
		               ifTrue: [ DRPhysicalRegister name: 'TempReg' ]
		               ifFalse: [ instruction result ].

	self twoOperandCogitRTL: aMnemonic operands: { 
			replacement.
			destination }.

	instruction result isMemoryAddress ifTrue: [ 
		self twoOperandCogitRTL: 'Move' operands: { 
				destination.
				(DRPhysicalRegister name: 'MEMORY0') } ]
]

{ #category : #visiting }
DRCogitCodeGenerator >> twoOperandCogitRTL: aMnemonic operands: operands [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: aMnemonic , operands first result rtlOperandQualifier
				 , operands second result rtlOperandQualifier
			 arguments: (self rtlArgumentsForValue: operands first)
				 , (self rtlArgumentsForValue: operands second))
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitAddition: aDRCogitAdd [ 

	^ self threeOperandCogitRTL: #Add instruction: aDRCogitAdd 
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitAnd: aDRAnd [

	"Use threeOperandCogitRTL: ?"
	^ self twoOperandCogitRTL: #And instruction: aDRAnd
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitBitAnd: aDRCogitBitAnd [ 

	^ self threeOperandCogitRTL: #And instruction: aDRCogitBitAnd

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

	self twoOperandCogitRTL: #Cmp operands: comparands
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitConditionalJump: aDRConditionalJump [ 

	"Generate both jumps, the fallthorugh will get ignored"
	self generateJump: aDRConditionalJump condition singleChild cogitTrueConditionCode target: aDRConditionalJump trueBranch.
	self generateJump: aDRConditionalJump condition singleChild cogitFalseConditionCode target: aDRConditionalJump falseBranch
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitConstantCondition: aDRConstantCondition [
	
	"If true, jump to the true branch.
	If false, jump to the false branch."
	
	"Code generated in the conditional jump"
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitCopy: aDRMove [

	"Do not generate moves of null values"
	aDRMove operands first isNullValue
		ifTrue: [ ^ self ].
	self twoOperandCogitRTL: #Move instruction: aDRMove
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
	^ self visitBitShift: (DRBitShift operands: aDRCogitLeftShift operands reversed result:  aDRCogitLeftShift result)
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

	"Only implement register byte accesses for now"

	| sizeSuffix |
	aDRLoad size = 1
		ifTrue: [ sizeSuffix := 'b' ]
		ifFalse: [ sizeSuffix := (aDRLoad size * 8) asString ].

	"Zero the register before loading a byte"
	self twoOperandCogitRTL: #Move operands: { 
			0 asDRValue.
			aDRLoad result }.

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: ('MoveM' , sizeSuffix , ':r:R:') asSymbol
			 arguments: { 
					 (self rtlExpressionForValue: 0 asDRValue).
					 (self rtlExpressionForValue: aDRLoad operand1).
					 (self rtlExpressionForValue: aDRLoad result) })
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
	
	self twoOperandCogitRTL: #Move instruction: aDRCogitNegate.
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

	"Use threeOperandCogitRTL: ?"
	^ self twoOperandCogitRTL: #Or instruction: aDROr
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
			 selector: #RetN:
			 arguments: { (RBMessageNode
					  receiver: (self rtlExpressionForValue: aDRReturn operand1)
					  selector: #*
					  arguments: { (RBMessageNode
							   receiver: (RBVariableNode named: 'objectMemory')
							   selector: #wordSize) }) })
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitRightShift: aDRCogitRightShift [ 

	^ self threeOperandCogitRTL: #LogicalShiftRight instruction: aDRCogitRightShift

]

{ #category : #visiting }
DRCogitCodeGenerator >> visitStore: aDRCogitStore [

	self twoOperandCogitRTL: #Move operands: { 
			aDRCogitStore operand1.
			aDRCogitStore result }
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
	aTest operands second isConstant
		ifTrue: [
			aTest replaceBy: aTest reversed.
			comparands := comparands reversed ].
	self twoOperandCogitRTL: #Tst operands: comparands
]

{ #category : #visiting }
DRCogitCodeGenerator >> visitUnsignedCoercion: aDRCogitUnsignedCoercion [ 
	
	"Nothing for now"
	^ self visitNullValue: aDRCogitUnsignedCoercion
]
