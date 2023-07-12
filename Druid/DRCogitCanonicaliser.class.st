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
DRCogitCanonicaliser >> visitAddition: aDRAdd [
	"Simplify res := a + 0 => res := a"

	(aDRAdd operands anySatisfy: [ :op | op = 0 asDRValue ]) ifTrue: [
		^ aDRAdd replaceBy: (DRCopy
				   operands: (aDRAdd operands select: [ :e | e ~= 0 asDRValue ])
				   result: aDRAdd result) ].

	"If it is a pattern such as res := ((a + b) + c"
	aDRAdd operands
		detect: [ :op |
			op isAdd and: [
				(op operands anySatisfy: [ :opop | opop isConstant ]) and: [
					op operands anySatisfy: [ :opop | opop isConstant not ] ] ] ]
		ifFound: [ :subtree |
			| variableOperand subConstantOperand secondSubtreeOperand |
			"We know one of the suboperands is a constant"
			subConstantOperand := subtree operands detect: [ :op |
				                      op isConstant ].
			variableOperand := subtree operands detect: [ :op |
				                   op isConstant not ].

			"Now get the other operand"
			secondSubtreeOperand := aDRAdd operands detect: [ :op |
				                        op ~~ subtree ].

			secondSubtreeOperand isConstant
				ifTrue: [ "Case 1) if b and c are constants, put constants together
					=> res := (a + (b + c))"
					(subConstantOperand respondsTo: #+) ifFalse: [
						| swap |
						swap := subConstantOperand.
						subConstantOperand := secondSubtreeOperand.
						secondSubtreeOperand := swap ].
					^ aDRAdd replaceBy: (DRAdd
							   operands: {
									   variableOperand.
									   (subConstantOperand value + secondSubtreeOperand value)
										   asDRValue }
							   result: aDRAdd result) ]
				ifFalse: [
					| newSubtree |
					"Case 2) if b is constant but c is not, take the constant out
					=> res := ((a + c) + b)"
					
					"Check if we can rewrite this instruction. If it may generate an overflow, do not"
					(aDRAdd users anySatisfy: [ :dependent |
						 dependent isConditionalBranch and: [
							 dependent hasOverflowCheck ] ]) ifTrue: [ "Do nothing" ^ self ].

					"Check if we can reuse the subtree result"
					subtree users size > 1 ifTrue: [ "Do nothing" ^ self ].

					newSubtree := DRAdd
						              operands: {
								              variableOperand.
								              secondSubtreeOperand }
						              result: subtree result.
					subtree removeFromCFG.
					aDRAdd addBefore: newSubtree.
					^ aDRAdd replaceBy: (DRAdd
							   operands: {
									   newSubtree.
									   subConstantOperand }
							   result: aDRAdd result) ] ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitAnd: aDRBitAnd [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitOr: aDRBitOr [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitXor: aDRBitXor [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBranchIfCondition: aDRBranchIfCondition [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBranchIfTrue: aDRBranchIfTrue [ 
	
	"Nothing"
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
DRCogitCanonicaliser >> visitContinueNextBytecode: aDRContinueNextBytecode [ 
	
	"Do nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitCopy: aDRCopy [ 
	
	"Nothing"
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
	(addressOperands first isLoadFramePointer and: [ 
		 addressOperands second isJITCompileTimeExpression:
			 'FrameReceiverOffset' ]) ifTrue: [ 
		^ aDRLoad replaceBy: (DRLoadReceiver result: aDRLoad result) ].

	"General optimisation:
	Simplify for now only patterns like
	r1 := r0 + c1.
	r2 := load r1
	=>
	r2 := load r0 + c1
	"
	offsetOperand := addressOperands
		                 detect: [ :op | op isConstant ]
		                 ifNone: [ ^ self ].
	baseOperand := addressOperands
		               detect: [ :op | op isConstant not ]
		               ifNone: [ ^ self ].
	
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

	"Nothing?"
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
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPop: aDRPop [

	"If I have no users, and my previous instruction is also a pop without users, merge us in a single PopMany instruction"

	| canBeMerged previous |
	previous := aDRPop previousInstruction ifNil: [ ^ self ].
	canBeMerged := aDRPop hasUsers not and: [ 
		               previous isPop and: [ previous hasUsers not ] ].
	canBeMerged ifTrue: [ 
		aDRPop previousInstruction removeFromCFG.
		aDRPop replaceBy: (DRPopMany
				 operands:
					 { (previous numberOfPoppedElements
					  + aDRPop numberOfPoppedElements) asDRValue }
				 result: DRNoRegister new) ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPopMany: aDRPopMany [ 
	
	^ self visitPop: aDRPopMany
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPush: aDRPush [

	| pushedValue |
	pushedValue := aDRPush operand1 simpleConstantFold.
	pushedValue isLoad ifFalse: [ ^ self ].
	pushedValue address isAbsolute ifTrue: [
		self flag: #FIXME.
		^ self ].
	aDRPush replaceDependency: pushedValue by: pushedValue address
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitReturn: aDRReturn [ 

	"Do nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitRightShift: aDRRightShift [ 
	
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
