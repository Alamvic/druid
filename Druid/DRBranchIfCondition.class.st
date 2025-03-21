Class {
	#name : 'DRBranchIfCondition',
	#superclass : 'DRInstruction',
	#instVars : [
		'trueBranch',
		'falseBranch',
		'backJumps',
		'condition'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBranchIfCondition >> acceptVisitor: aVisitor [

	^ aVisitor visitBranchIfCondition: self
]

{ #category : 'converting' }
DRBranchIfCondition >> asConditionalLoop [

	^ self controlFlowGraph allLoops
		  detect: [ :loop | loop loopConditional = self ]
		  ifNone: nil
]

{ #category : 'accessing' }
DRBranchIfCondition >> backJumpTargets [
	^ backJumps.
]

{ #category : 'accessing' }
DRBranchIfCondition >> beBackJumpTo: aDRBasicBlock [

	backJumps add: aDRBasicBlock
]

{ #category : 'accessing' }
DRBranchIfCondition >> beForwardJumpTo: aDRBasicBlock [

	backJumps remove: aDRBasicBlock ifAbsent: nil
]

{ #category : 'accessing' }
DRBranchIfCondition >> condition [
	
	^ condition
]

{ #category : 'accessing' }
DRBranchIfCondition >> condition: aCondition [

	condition := aCondition
]

{ #category : 'jit-compile-time' }
DRBranchIfCondition >> conditionAsJitCompileTimeExpression [

	| fullExpression |
	fullExpression := self isEqualsThanTrueComparison
		                  ifTrue: [
		                  self nonTrueOperand asJitCompileTimeExpression ]
		                  ifFalse: [
			                  self operand1 result name
			                  , self condition asJitCompileTimeExpressionString
			                  , self operand2 result name ].

	^ fullExpression asDRValue asJitCompileTimeExpression
]

{ #category : 'accessing' }
DRBranchIfCondition >> falseBranch [
	^ falseBranch
]

{ #category : 'accessing' }
DRBranchIfCondition >> falseBranch: aDRBasicBlock [ 

	backJumps remove: falseBranch ifAbsent: [  ].
	falseBranch := aDRBasicBlock
	
]

{ #category : 'accessing' }
DRBranchIfCondition >> hasOverflowCheck [

	^ condition isOverflow or: [ condition isNoOverflow ]
]

{ #category : 'initialization' }
DRBranchIfCondition >> initialize [

	super initialize.
	result := DRNoRegister new.
	backJumps := Set new
]

{ #category : 'testing' }
DRBranchIfCondition >> isBackJump [

	^ backJumps isNotEmpty
]

{ #category : 'testing' }
DRBranchIfCondition >> isBackJumpTo: aDRBasicBlock [ 
	
	^ backJumps includes: aDRBasicBlock
]

{ #category : 'testing' }
DRBranchIfCondition >> isConditionalBranch [

	^ true
]

{ #category : 'testing' }
DRBranchIfCondition >> isConditionalLoop [

	^ self asConditionalLoop isNotNil
	

]

{ #category : 'testing' }
DRBranchIfCondition >> isEqualsThanTrueComparison [

	^ self condition isEqualsThanComparison and: [ self operands includes: true asDRValue ]
]

{ #category : 'testing' }
DRBranchIfCondition >> isJITCompileTimeExpression [

	^ operands allSatisfy: [ :e | e isStaged ]
]

{ #category : 'accessing' }
DRBranchIfCondition >> isJump [ 

	^ true
]

{ #category : 'testing' }
DRBranchIfCondition >> isMandatoryInstruction [

	^ true
]

{ #category : 'building' }
DRBranchIfCondition >> newFalseBranch: aDRBasicBlock [

	aDRBasicBlock addPredecessor: self basicBlock.
	^ self falseBranch: aDRBasicBlock
]

{ #category : 'building' }
DRBranchIfCondition >> newTrueBranch: aDRBasicBlock [

	aDRBasicBlock addPredecessor: self basicBlock.
	^ self trueBranch: aDRBasicBlock
]

{ #category : 'accessing' }
DRBranchIfCondition >> nonTrueOperand [

	self operand1 = true asDRValue ifFalse: [ ^ self operand1 ].

	self assert: self operand2 ~= true asDRValue.
	^ self operand2
]

{ #category : 'printing' }
DRBranchIfCondition >> opcode [
	
	^ 'If'
]

{ #category : 'accessing' }
DRBranchIfCondition >> operands: operandList [

	condition := operandList first value.
	super operands: operandList allButFirst
]

{ #category : 'copying' }
DRBranchIfCondition >> postCopy [

	super postCopy.
	backJumps := backJumps copy
]

{ #category : 'visiting' }
DRBranchIfCondition >> prettyConditionPrint [

	^ self condition prettyPrintWithBranch: self
]

{ #category : 'printing' }
DRBranchIfCondition >> prettyPrint [

	^ (self isStaged
		   ifTrue: [ '[STAGED] ' ]
		   ifFalse: [ '' ]) , 'Jump if ' , self prettyConditionPrint
	  , ' -> ' , (self trueBranch ifNotNil: [ :br | br id ]) asString
	  , ' if false -> '
	  , (self falseBranch ifNotNil: [ :br | br id ]) asString
]

{ #category : 'replacing' }
DRBranchIfCondition >> replaceTarget: aDRBasicBlock by: aDRBasicBlock2 [

	trueBranch = aDRBasicBlock ifTrue: [ 
		self trueBranch: aDRBasicBlock2 ].
	falseBranch = aDRBasicBlock ifTrue: [ 
		self falseBranch: aDRBasicBlock2 ]
]

{ #category : 'highlighting' }
DRBranchIfCondition >> reverse [

	self reverseOperands.
	self condition: self condition reversed
]

{ #category : 'transforming' }
DRBranchIfCondition >> reversed [

	^ DRBranchIfCondition new
		operands: { self condition reversed }, self operands reversed;
		trueBranch: self trueBranch;
		falseBranch: self falseBranch;
		yourself
]

{ #category : 'SCCP' }
DRBranchIfCondition >> sccpEvaluateConditionFor: sccp [

	^ self condition
		sccpEvaluateFor: sccp
		branch: self
]

{ #category : 'SCCP' }
DRBranchIfCondition >> sccpEvaluateFor: sccp [

	| conditionLattice jumpTarget |
	conditionLattice := self sccpEvaluateConditionFor: sccp.

	"If we know nothing, do nothing yet"
	conditionLattice = sccp top ifTrue: [ ^ self ].

	"If we know it's a boolean constant, only evaluate the branch we care about"
	(conditionLattice ~= sccp bottom and: [
		 conditionLattice isConstant and: [
			 conditionLattice type isBooleanType ] ]) ifTrue: [ "otherwise add the true or false one"
		jumpTarget := conditionLattice value
			              ifTrue: [ self trueBranch ]
			              ifFalse: [ self falseBranch ].
		sccp
			addToEdgeCFGWorklistConditionalJump: self
			targetting: jumpTarget.
		^ self ].

	"Otherwise, we know that we don't know, add both branch targets"
	self targets do: [ :t |
		sccp addToEdgeCFGWorklistConditionalJump: self targetting: t ]
]

{ #category : 'SCCP' }
DRBranchIfCondition >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'accessing' }
DRBranchIfCondition >> targets [

	^ { trueBranch . falseBranch }
]

{ #category : 'accessing' }
DRBranchIfCondition >> trueBranch [

	^ trueBranch
]

{ #category : 'accessing' }
DRBranchIfCondition >> trueBranch: aDRBasicBlock [

	backJumps remove: trueBranch ifAbsent: [  ].

	trueBranch := aDRBasicBlock
]
