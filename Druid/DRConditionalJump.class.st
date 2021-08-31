Class {
	#name : #DRConditionalJump,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRConditionalJump class >> operands: operands [

	^ self operands: operands result: DRNoRegister new
]

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

{ #category : #accessing }
DRConditionalJump >> isConditionalJump [ 

	^ true
]

{ #category : #accessing }
DRConditionalJump >> isJump [ 

	^ true
]

{ #category : #printing }
DRConditionalJump >> prettyPrint [

	^ 'Jump if' , self condition result name , ' true -> '
	  , self trueBranch id asString , ' if false -> '
	  , self falseBranch id asString
]

{ #category : #accessing }
DRConditionalJump >> sccpEvaluateFor: sccp [

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
