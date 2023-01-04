Class {
	#name : #DRAbstractBranch,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch',
		'backJumps'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRAbstractBranch >> backJumpTargets [
	^ backJumps.
]

{ #category : #accessing }
DRAbstractBranch >> beBackJumpTo: aDRBasicBlock [

	backJumps add: aDRBasicBlock
]

{ #category : #accessing }
DRAbstractBranch >> beForwardJumpTo: aDRBasicBlock [

	backJumps remove: aDRBasicBlock
]

{ #category : #accessing }
DRAbstractBranch >> falseBranch [
	^ falseBranch
]

{ #category : #accessing }
DRAbstractBranch >> falseBranch: aDRBasicBlock [ 

	backJumps remove: falseBranch ifAbsent: [  ].
	falseBranch := aDRBasicBlock
	
]

{ #category : #initialization }
DRAbstractBranch >> initialize [

	super initialize.
	result := DRNoRegister new.
	backJumps := Set new
]

{ #category : #testing }
DRAbstractBranch >> isBackJump [

	^ backJumps isNotEmpty
]

{ #category : #testing }
DRAbstractBranch >> isBackJumpTo: aDRBasicBlock [ 
	
	^ backJumps includes: aDRBasicBlock
]

{ #category : #testing }
DRAbstractBranch >> isConditionalBranch [

	^ true
]

{ #category : #accessing }
DRAbstractBranch >> isJump [ 

	^ true
]

{ #category : #building }
DRAbstractBranch >> newFalseBranch: aDRBasicBlock [

	aDRBasicBlock addPredecessor: self basicBlock.
	^ self falseBranch: aDRBasicBlock
]

{ #category : #building }
DRAbstractBranch >> newTrueBranch: aDRBasicBlock [

	aDRBasicBlock addPredecessor: self basicBlock.
	^ self trueBranch: aDRBasicBlock
]

{ #category : #printing }
DRAbstractBranch >> opcode [
	
	^ 'If'
]

{ #category : #copying }
DRAbstractBranch >> postCopy [

	super postCopy.
	backJumps := backJumps copy
]

{ #category : #printing }
DRAbstractBranch >> prettyPrint [

	^ 'Jump if ' , self prettyConditionPrint , ' -> '
	  , (self trueBranch ifNotNil: [ :br | br id ]) asString
	  , ' if false -> '
	  , (self falseBranch ifNotNil: [ :br | br id ]) asString
]

{ #category : #replacing }
DRAbstractBranch >> replaceTarget: aDRBasicBlock by: aDRBasicBlock2 [

	trueBranch = aDRBasicBlock ifTrue: [ 
		self trueBranch: aDRBasicBlock2 ].
	falseBranch = aDRBasicBlock ifTrue: [ 
		self falseBranch: aDRBasicBlock2 ]
]

{ #category : #SCCP }
DRAbstractBranch >> sccpEvaluateFor: sccp [

	| conditionLattice jumpTarget |
	conditionLattice := self sccpEvaluateConditionFor: sccp.

	"If we know nothing, do nothing yet"
	conditionLattice = sccp top ifTrue: [ ^ self ].

	"If we know that we don't know, add both branch targets"
	conditionLattice = sccp bottom ifTrue: [ 
		self targets do: [ :t | 
			sccp addToEdgeCFGWorklistConditionalJump: self targetting: t ].
		^ self ].

	"otherwise add the true or false one"
	jumpTarget := conditionLattice value
		              ifTrue: [ self trueBranch ]
		              ifFalse: [ self falseBranch ].
	sccp addToEdgeCFGWorklistConditionalJump: self targetting: jumpTarget
]

{ #category : #SCCP }
DRAbstractBranch >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #accessing }
DRAbstractBranch >> targets [

	^ { trueBranch . falseBranch }
]

{ #category : #accessing }
DRAbstractBranch >> trueBranch [

	^ trueBranch
]

{ #category : #accessing }
DRAbstractBranch >> trueBranch: aDRBasicBlock [

	backJumps remove: trueBranch ifAbsent: [  ].

	trueBranch := aDRBasicBlock
]
