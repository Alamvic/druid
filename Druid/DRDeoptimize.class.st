Class {
	#name : #DRDeoptimize,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRDeoptimize >> acceptVisitor: aDRCogitCanonicaliser [ 
	
	aDRCogitCanonicaliser visitDeoptimize: self
]

{ #category : #visiting }
DRDeoptimize >> isMandatoryInstruction [

	^ true
]

{ #category : #SCCP }
DRDeoptimize >> sccpEvaluateFor: sccp [

	"Nothing to do"
]
