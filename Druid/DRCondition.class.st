Class {
	#name : #DRCondition,
	#superclass : #DRInstruction,
	#instVars : [
		'trueBranch',
		'falseBranch'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRCondition >> falseBranch [
	^ falseBranch
]

{ #category : #accessing }
DRCondition >> falseBranch: aDruidIRGenerator [ 

	falseBranch := aDruidIRGenerator 
]

{ #category : #accessing }
DRCondition >> targets [

	^ { trueBranch . falseBranch }
]

{ #category : #accessing }
DRCondition >> trueBranch: aDruidIRGenerator [ 
	
	trueBranch := aDruidIRGenerator 
]
