Class {
	#name : #DRBranchIfCondition,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch',
		'backJumps',
		'condition'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRBranchIfCondition >> acceptVisitor: aVisitor [

	^ aVisitor visitBranchIfCondition: self
]

{ #category : #accessing }
DRBranchIfCondition >> backJumpTargets [
	^ backJumps.
]

{ #category : #accessing }
DRBranchIfCondition >> beBackJumpTo: aDRBasicBlock [

	backJumps add: aDRBasicBlock
]

{ #category : #accessing }
DRBranchIfCondition >> beForwardJumpTo: aDRBasicBlock [

	backJumps remove: aDRBasicBlock
]

{ #category : #accessing }
DRBranchIfCondition >> condition [
	
	^ condition
]

{ #category : #accessing }
DRBranchIfCondition >> condition: aCondition [

	condition := aCondition
]

{ #category : #'jit-compile-time' }
DRBranchIfCondition >> conditionAsJitCompileTimeExpression [

	| fullExpression |
	fullExpression := self operand1 asJitCompileTimeExpression expression
	                  , self condition asJitCompileTimeExpressionString
	                  , self operand2 asJitCompileTimeExpression expression.

	^ fullExpression asDRValue asJitCompileTimeExpression
]

{ #category : #accessing }
DRBranchIfCondition >> falseBranch [
	^ falseBranch
]

{ #category : #accessing }
DRBranchIfCondition >> falseBranch: aDRBasicBlock [ 

	backJumps remove: falseBranch ifAbsent: [  ].
	falseBranch := aDRBasicBlock
	
]

{ #category : #accessing }
DRBranchIfCondition >> hasOverflowCheck [

	^ condition isOverflow
]

{ #category : #initialization }
DRBranchIfCondition >> initialize [

	super initialize.
	result := DRNoRegister new.
	backJumps := Set new
]

{ #category : #testing }
DRBranchIfCondition >> isBackJump [

	^ backJumps isNotEmpty
]

{ #category : #testing }
DRBranchIfCondition >> isBackJumpTo: aDRBasicBlock [ 
	
	^ backJumps includes: aDRBasicBlock
]

{ #category : #testing }
DRBranchIfCondition >> isConditionalBranch [

	^ true
]

{ #category : #testing }
DRBranchIfCondition >> isJITCompileTimeExpression [

	^ operands allSatisfy: [ :e | e isJITCompileTimeExpression ]
]

{ #category : #accessing }
DRBranchIfCondition >> isJump [ 

	^ true
]

{ #category : #testing }
DRBranchIfCondition >> isMandatoryInstruction [

	^ true
]

{ #category : #building }
DRBranchIfCondition >> newFalseBranch: aDRBasicBlock [

	aDRBasicBlock addPredecessor: self basicBlock.
	^ self falseBranch: aDRBasicBlock
]

{ #category : #building }
DRBranchIfCondition >> newTrueBranch: aDRBasicBlock [

	aDRBasicBlock addPredecessor: self basicBlock.
	^ self trueBranch: aDRBasicBlock
]

{ #category : #printing }
DRBranchIfCondition >> opcode [
	
	^ 'If'
]

{ #category : #accessing }
DRBranchIfCondition >> operands: operandList [

	condition := operandList first value.
	super operands: operandList allButFirst
]

{ #category : #copying }
DRBranchIfCondition >> postCopy [

	super postCopy.
	backJumps := backJumps copy
]

{ #category : #visiting }
DRBranchIfCondition >> prettyConditionPrint [

	^ self condition prettyPrintWithBranch: self
]

{ #category : #printing }
DRBranchIfCondition >> prettyPrint [

	^ 'Jump if ' , self prettyConditionPrint , ' -> '
	  , (self trueBranch ifNotNil: [ :br | br id ]) asString
	  , ' if false -> '
	  , (self falseBranch ifNotNil: [ :br | br id ]) asString
]

{ #category : #replacing }
DRBranchIfCondition >> replaceTarget: aDRBasicBlock by: aDRBasicBlock2 [

	trueBranch = aDRBasicBlock ifTrue: [ 
		self trueBranch: aDRBasicBlock2 ].
	falseBranch = aDRBasicBlock ifTrue: [ 
		self falseBranch: aDRBasicBlock2 ]
]

{ #category : #transforming }
DRBranchIfCondition >> reversed [

	^ DRBranchIfCondition new
		operands: { self condition reversed }, self operands reversed;
		trueBranch: self trueBranch;
		falseBranch: self falseBranch;
		yourself
]

{ #category : #SCCP }
DRBranchIfCondition >> sccpEvaluateConditionFor: sccp [

	^ self condition
		sccpEvaluateFor: sccp
		branch: self
]

{ #category : #SCCP }
DRBranchIfCondition >> sccpEvaluateFor: sccp [

	| conditionLattice jumpTarget |
	conditionLattice := self sccpEvaluateConditionFor: sccp.

	"If we know nothing, do nothing yet"
	conditionLattice = sccp top ifTrue: [ ^ self ].

	"If we know that we don't know, add both branch targets"
	(conditionLattice = sccp bottom or: [
		 (conditionLattice isConstant and: [ conditionLattice type isBooleanType ]) not ])
		ifTrue: [
			self targets do: [ :t | sccp addToEdgeCFGWorklistConditionalJump: self targetting: t ].
			^ self ].

	"otherwise add the true or false one"
	jumpTarget := conditionLattice value
		              ifTrue: [ self trueBranch ]
		              ifFalse: [ self falseBranch ].
	sccp addToEdgeCFGWorklistConditionalJump: self targetting: jumpTarget
]

{ #category : #SCCP }
DRBranchIfCondition >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #accessing }
DRBranchIfCondition >> targets [

	^ { trueBranch . falseBranch }
]

{ #category : #accessing }
DRBranchIfCondition >> trueBranch [

	^ trueBranch
]

{ #category : #accessing }
DRBranchIfCondition >> trueBranch: aDRBasicBlock [

	backJumps remove: trueBranch ifAbsent: [  ].

	trueBranch := aDRBasicBlock
]
