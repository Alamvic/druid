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
DRConditionalJump >> falseBranch [
	^ falseBranch
]

{ #category : #accessing }
DRConditionalJump >> falseBranch: aDruidIRGenerator [ 

	falseBranch := aDruidIRGenerator 
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
