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
DRAbstractBranch >> beBackJumpTo: aDRBasicBlock [ 
	
	backJumps add: aDRBasicBlock 
]

{ #category : #accessing }
DRAbstractBranch >> falseBranch [
	^ falseBranch
]

{ #category : #accessing }
DRAbstractBranch >> falseBranch: aDruidIRGenerator [ 

	falseBranch := aDruidIRGenerator 
]

{ #category : #initialization }
DRAbstractBranch >> initialize [

	super initialize.
	result := DRNoRegister new.
	backJumps := OrderedCollection new
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

{ #category : #printing }
DRAbstractBranch >> opcode [
	
	^ 'If'
]

{ #category : #printing }
DRAbstractBranch >> prettyPrint [

	^ 'Jump if ' , self prettyConditionPrint , ' -> '
	  , self trueBranch id asString , ' if false -> '
	  , self falseBranch id asString
]

{ #category : #replacing }
DRAbstractBranch >> replaceTarget: aDRBasicBlock by: aDRBasicBlock2 [

	trueBranch = aDRBasicBlock ifTrue: [ trueBranch := aDRBasicBlock2 ].
	falseBranch = aDRBasicBlock ifTrue: [ falseBranch := aDRBasicBlock2 ].

	(backJumps includes: aDRBasicBlock) ifTrue: [ 
		backJumps remove: aDRBasicBlock ]
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
DRAbstractBranch >> trueBranch: aDruidIRGenerator [ 
	
	trueBranch := aDruidIRGenerator 
]
