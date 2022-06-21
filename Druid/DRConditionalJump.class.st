Class {
	#name : #DRConditionalJump,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch',
		'backJumpTo'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRConditionalJump >> acceptVisitor: aVisitor [

	^ aVisitor visitConditionalJump: self
]

{ #category : #accessing }
DRConditionalJump >> beBackJumpTo: aDRBasicBlock [

	backJumpTo add: aDRBasicBlock
]

{ #category : #accessing }
DRConditionalJump >> beForwardJumpTo: aDRBasicBlock [ 
	
	backJumpTo remove: aDRBasicBlock
]

{ #category : #accessing }
DRConditionalJump >> condition [
	
	^ operands first
]

{ #category : #accessing }
DRConditionalJump >> condition: aCondition [

	operands at: 1 put: aCondition
]

{ #category : #accessing }
DRConditionalJump >> falseBranch [
	^ falseBranch
]

{ #category : #accessing }
DRConditionalJump >> falseBranch: aDRBasicBlock [

	backJumpTo remove: falseBranch ifAbsent: [  ].

	falseBranch := aDRBasicBlock
]

{ #category : #initialization }
DRConditionalJump >> initialize [

	super initialize.
	result := DRNoRegister new.
	backJumpTo := Set new
]

{ #category : #testing }
DRConditionalJump >> isBackJump [
	
	^ backJumpTo isNotEmpty 
]

{ #category : #testing }
DRConditionalJump >> isBackJumpTo: aDRBasicBlock [

	^ backJumpTo includes: aDRBasicBlock
]

{ #category : #testing }
DRConditionalJump >> isConditionalBranch [

	^ true
]

{ #category : #accessing }
DRConditionalJump >> isConditionalJump [ 

	^ true
]

{ #category : #accessing }
DRConditionalJump >> isJump [ 

	^ true
]

{ #category : #printing }
DRConditionalJump >> opcode [
	
	^ 'If'
]

{ #category : #copying }
DRConditionalJump >> postCopy [

	super postCopy.
	backJumpTo := backJumpTo copy.
]

{ #category : #printing }
DRConditionalJump >> prettyConditionPrint [

	^ self condition result name , ' true'
]

{ #category : #printing }
DRConditionalJump >> prettyPrint [

	^ 'Jump if ' , self prettyConditionPrint , ' -> '
	  , self trueBranch id asString , ' if false -> '
	  , self falseBranch id asString
]

{ #category : #replacing }
DRConditionalJump >> replaceTarget: aDRBasicBlock by: aDRBasicBlock2 [

	trueBranch = aDRBasicBlock ifTrue: [ self trueBranch: aDRBasicBlock2 ].
	falseBranch = aDRBasicBlock ifTrue: [ self falseBranch: aDRBasicBlock2 ]
]

{ #category : #SCCP }
DRConditionalJump >> sccpEvaluateFor: sccp [

	| condition jumpTarget |
	condition := self condition sccpLatticeValueFor: sccp.

	"If we know nothing, do nothing yet"
	condition = sccp top ifTrue: [ ^ self ].

	condition = sccp bottom ifTrue: [ 
		self targets do: [ :t | 
			sccp addToEdgeCFGWorklistConditionalJump: self targetting: t ].
		^ self ].

	condition isNullValue ifTrue: [ ^ self ].

	jumpTarget := condition value
		              ifTrue: [ self trueBranch ]
		              ifFalse: [ self falseBranch ].
	sccp addToEdgeCFGWorklistConditionalJump: self targetting: jumpTarget
]

{ #category : #SCCP }
DRConditionalJump >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #accessing }
DRConditionalJump >> targets [

	^ { trueBranch . falseBranch }
]

{ #category : #accessing }
DRConditionalJump >> trueBranch [

	^ trueBranch
]

{ #category : #accessing }
DRConditionalJump >> trueBranch: aDRBasicBlock [

	backJumpTo remove: trueBranch ifAbsent: [  ].

	trueBranch := aDRBasicBlock
]
