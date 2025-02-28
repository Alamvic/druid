Class {
	#name : 'DRDeoptimize',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRDeoptimize >> acceptVisitor: aDRCogitCanonicaliser [ 
	
	aDRCogitCanonicaliser visitDeoptimize: self
]

{ #category : 'visiting' }
DRDeoptimize >> isMandatoryInstruction [

	^ true
]

{ #category : 'testing' }
DRDeoptimize >> needsStackFlush [

	"Stack must be flushed to jump to interpreted mode"
	^ true
]

{ #category : 'SCCP' }
DRDeoptimize >> sccpEvaluateFor: sccp [

	"Nothing to do"
]
