Class {
	#name : #DRConditionalJump,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRConditionalJump >> acceptVisitor: aVisitor [

	^ aVisitor visitConditionalJump: self
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
DRConditionalJump >> falseBranch: aDruidIRGenerator [ 

	falseBranch := aDruidIRGenerator 
]

{ #category : #initialization }
DRConditionalJump >> initialize [

	super initialize.
	result := DRNoRegister new
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
	
	trueBranch = aDRBasicBlock ifTrue: [
		trueBranch := aDRBasicBlock2 ].
	falseBranch = aDRBasicBlock ifTrue: [
		falseBranch := aDRBasicBlock2 ].
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
DRConditionalJump >> trueBranch: aDruidIRGenerator [ 
	
	trueBranch := aDruidIRGenerator 
]
