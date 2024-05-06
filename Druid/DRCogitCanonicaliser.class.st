Class {
	#name : #DRCogitCanonicaliser,
	#superclass : #DROptimisation,
	#category : #'Druid-Cogit'
}

{ #category : #accessing }
DRCogitCanonicaliser >> applyTo: cfg [

	cfg instructions copy do: [ :e | e acceptVisitor: self ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> sortOperandsInDoubleInstructionIfPossible: aDRBinaryInstruction [
	"If it is a pattern such as ((var op const) op X)
	It is replaced by:
		((X op const) op var) -> if X is constant
		((var op X) op const) -> otherwise
	"

	| newSubtree |
	aDRBinaryInstruction operands
		detect: [ :op |
			op class = aDRBinaryInstruction class and: [ "Operands sorting is only possible if aDRBinaryInstruction is the only user of the subtree"
				op users size = 1 and: [
					(op operands anySatisfy: [ :opop | opop isConstant ]) and: [
						op operands anySatisfy: [ :opop | opop isConstant not ] ] ] ] ]
		ifFound: [ :subtree |
			| subConstant otherOperand subVariable |
			"We know one of the suboperands is a constant and the other is not"
			subConstant := subtree operands detect: [ :op | op isConstant ].
			subVariable := subtree operands detect: [ :op | op isConstant not ].

			"Now get the other operand (not the subtree)"
			otherOperand := aDRBinaryInstruction operands detect: [ :op |
				                op ~~ subtree ].

			"Check if we can rewrite this instruction. If it may generate an overflow, do not"
			(aDRBinaryInstruction users anySatisfy: [ :dependent |
				 dependent isConditionalBranch and: [ dependent hasOverflowCheck ] ])
					ifTrue: [ "Do nothing" ^ self ].

			otherOperand isConstant
				ifTrue: [ "There are two constants and one variable, put the constants together"
					subtree replaceOperand: subVariable by: otherOperand.
					^ aDRBinaryInstruction
						  replaceOperand: otherOperand
						  by: subVariable ]
				ifFalse: [ "There are two variables and one constants, put the variables together"
					newSubtree := aDRBinaryInstruction class
						              operands: {
								              subVariable.
								              otherOperand }
						              result: subtree result.
					subtree removeFromCFG.
					"We should create a new subtree next to the instruction due to dominance constratints"
					aDRBinaryInstruction addBefore: newSubtree.
					^ aDRBinaryInstruction replaceBy: (aDRBinaryInstruction class
							   operands: {
									   newSubtree.
									   subConstant }
							   result: aDRBinaryInstruction result) ] ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> transformMultiplyByShiftIfPossible: aDRMultiply [

	| exponent shiftInstr |
	(aDRMultiply operand2 isConstant and: [
		 aDRMultiply operand2 value even ]) ifTrue: [
		exponent := (aDRMultiply operand2 value log: 2) asInteger.
		shiftInstr := DRLogicalLeftShift
			              operands: {
					              aDRMultiply operand1.
					              exponent asDRValue }
			              result: aDRMultiply result.

		aDRMultiply replaceBy: shiftInstr ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitAddition: aDRAdd [
	"Simplify res := a + 0 => res := a"

	(aDRAdd operands anySatisfy: [ :op | op = 0 asDRValue ]) ifTrue: [
		^ aDRAdd replaceBy: (DRCopy
				   operands: (aDRAdd operands select: [ :e | e ~= 0 asDRValue ])
				   result: aDRAdd result) ].

	self sortOperandsInDoubleInstructionIfPossible: aDRAdd 
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitAnnnotateBytecode: aDRAnnotateBytecode [ 

	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitAnd: aDRBitAnd [

	self sortOperandsInDoubleInstructionIfPossible: aDRBitAnd
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitOr: aDRBitOr [ 
	
	self sortOperandsInDoubleInstructionIfPossible: aDRBitOr
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitXor: aDRBitXor [ 
	
	self sortOperandsInDoubleInstructionIfPossible: aDRBitXor 
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBranchIfCondition: aDRConditionalBranch [

	aDRConditionalBranch operands
		detect: [ :op | op isPhiFunction and: [ op operands size > 1 ] ]
		ifFound: [ :thePhi |
			(thePhi operands allSatisfy: [ :op | op isSetConditionCode ])
				ifTrue: [ thePhi basicBlock tailDuplicate ] ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBytecodeJump: aDRBytecodeJump [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitCall: aDRCall [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitClosureActivation: aDRClosureActivation [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitClosureCreation: aDRClosureCreation [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitCogitSendMarshall: aDRCogitSendMarshall [ 
	
	"Do nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitContinueNextBytecode: aDRContinueNextBytecode [ 
	
	"Do nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitCopy: aDRCopy [
	"Remove instructions of the form: 
		ReceiverResultReg := LoadReceiver"

	(aDRCopy operand1 isLoadReceiver and: [
		 aDRCopy result name = 'ReceiverResultReg' and: [
			 aDRCopy users isEmpty ] ]) ifTrue: [ aDRCopy removeFromCFG ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitDivision: aDRDivision [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitFloatToIntConversion: aDRFloatToInt [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitFlushStack: aDRFlushStack [
	"Nothing"

	
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitGetConditionCode: aDRGetConditionCode [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitGetFloatBits: aDRGetFloatBits [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitGetFloatFromBits: aDRGetFloatFromBits [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitIntToFloatConversion: aDRIntToFloat [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitJITMessageSend: aDRMessageSend [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitJump: aDRJump [ 
	
	"Nothing for jumps"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLeftRotate: aDRRotate [ 
	^ self
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLeftShift: aDRLeftShift [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoad: aDRLoad [

	| address addressOperands offsetOperand baseOperand addressExpression |
	address := aDRLoad address.
	(address isAbsolute and: [ address expression isAdd ]) ifFalse: [ ^ self ].
	addressExpression := address expression.

	"Cogit Specific optimisation: collapse
		r0 := load framePointerRegister + receiver offset
	into
		r0 := load receiverRegister"
	addressOperands := addressExpression operands.
	(addressOperands first simpleConstantFold asDRValue isLoadFramePointer and: [
		 addressOperands second simpleConstantFold asDRValue isJITCompileTimeExpression:
			 'FrameReceiverOffset' ]) ifTrue: [
		^ aDRLoad replaceBy: (DRLoadReceiver result: aDRLoad result) ].

	"General optimisation:
	Simplify for now only patterns like
	r1 := r0 + c1.
	r2 := load r1
	=>
	r2 := load r0 + c1
	"
	offsetOperand := addressOperands detect: [ :op | op isConstant ] ifNone: [ ^ self ].
	baseOperand := addressOperands detect: [ :op | op isConstant not ] ifNone: [ ^ self ].

	^ aDRLoad replaceOperand: address by: (DRBaseOffsetMemoryAddress new
			   base: baseOperand;
			   offset: offsetOperand;
			   yourself)
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadArgument: aDRLoadArgument [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadReceiver: aDRLoadReceiver [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadStackPointer: aDRLoadStackPointer [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadStackValue: aDRLoadStackValue [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitMessageSend: aDRMessageSend [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitMod: aDRMod [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitMultiply: aDRMultiply [
	"If possible, transform the multiplication by a shift"
	self transformMultiplyByShiftIfPossible: aDRMultiply.

	"If possible, put the constants (or the variables) together"
	self sortOperandsInDoubleInstructionIfPossible: aDRMultiply
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitNegate: aDRNegate [ 

	"Nothing?"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitNoop: aDRNoop [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPhiFunction: aDRPhiFunction [

	| copy |
	aDRPhiFunction operands size = 1 ifTrue: [ ^ self ].
	aDRPhiFunction basicBlock isExitBlock ifTrue: [ ^ self ].

	(aDRPhiFunction users anySatisfy: [ :user |
		 user isSetConditionCode or: [ user isConditionalBranch ] ])
		ifFalse: [ ^ self ].

	"If we don't do this, we might end up in a situation where the Phi 'moves down' in the CFG and after some optimizations, we will have a BasicBlock that jumps to itself. For the moment, this particular scenario is not supported in the IR->Cogit translation phase (it generates an infinite loop)."
	aDRPhiFunction basicBlock hasIncomingBackjumps ifTrue: [ ^ self ].


	copy := aDRPhiFunction operands
		        detect: [ :op | op isCopy and: [ op operand isConstant ] ]
		        ifNone: [ ^ self ].

	aDRPhiFunction basicBlock splitTo:
		(aDRPhiFunction predecessorAtOperand: copy)
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPop: aDRPop [

	| canBeMerged previous |
	previous := aDRPop previousInstruction ifNil: [ ^ self ].
	canBeMerged := aDRPop hasUsers not and: [
		               previous isPop and: [
			               previous hasUsers not and: [
				               previous isUnspilled = aDRPop isUnspilled ] ] ].
	canBeMerged ifFalse: [ ^ self ].
	aDRPop previousInstruction removeFromCFG.
	aDRPop replaceBy: (DRPop
			 operands: {
					 (previous numberOfPoppedElements + aDRPop numberOfPoppedElements)
						 asDRValue.
					 aDRPop isUnspilled }
			 result: DRNoRegister new)
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPush: aDRPush [

	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitReturn: aDRReturn [ 

	"Do nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitSignedRightShift: aDRRightShift [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitStore: aDRStore [ 
	
	"General optimisation:
	Simplify for now only patterns like
	r1 := r0 + c1.
	r2 := store value r1
	=>
	r2 := store value r0 + c1
	"
	| address addressExpression addressOperands offsetOperand baseOperand |

	address := aDRStore address.
	(address isAbsolute and: [ address expression isAdd ]) ifFalse: [ ^ self ].

	addressExpression := address expression.
	addressOperands := addressExpression operands.
	offsetOperand := addressOperands
		                 detect: [ :op | op isConstant ]
		                 ifNone: [ ^ self ].
	baseOperand := addressOperands
		               detect: [ :op | op isConstant not ]
		               ifNone: [ ^ self ].
	
	^ aDRStore replaceOperand: address by: (DRBaseOffsetMemoryAddress new
		base: baseOperand;
		offset: offsetOperand;
		yourself)
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitStoreTempVar: aDRStoreTemporaryVariable [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitSubstract: aDRSubtract [
	"Replace a subtractions of a constant by an addition to simplify transformations"

	(aDRSubtract subtrahend isConstant and: [
		 aDRSubtract users allSatisfy: [ :e |
			 e isAdd or: [ e isSubtract or: [ e isLoad or: [ e isStore ] ] ] ] ])
		ifTrue: [
			aDRSubtract replaceBy: (DRAdd
					 operands: {
							 aDRSubtract subtrahend value negated asDRValue.
							 aDRSubtract minuend }
					 result: aDRSubtract result) ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitTemporaryVariable: aDRLoadTemporaryVariable [ 

	"Nothing?"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitUnsignedCoercion: aDRUnsignedCoercion [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitUnsignedRightShift: aDRRightShift [ 
	
	"Nothing"
]
