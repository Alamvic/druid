Class {
	#name : 'DRInitializePragma',
	#superclass : 'DRInstruction',
	#instVars : [
		'pragmas',
		'scope'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'queries' }
DRInitializePragma class >> pragmas [

	^ self result: DRNoRegister new.
	
]

{ #category : 'visiting' }
DRInitializePragma >> acceptVisitor: aVisitor [

	aVisitor visitInitializePragma: self
]

{ #category : 'visiting' }
DRInitializePragma >> isMandatoryInstruction [

	^ true
]

{ #category : 'accessing' }
DRInitializePragma >> pragmas [

	^ pragmas
]

{ #category : 'accessing' }
DRInitializePragma >> pragmas: anObject [

	pragmas := anObject
]

{ #category : 'visiting' }
DRInitializePragma >> primitiveFromPragma [ 

	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asIRPrimitive ]
		ifNone: [ OCIRPrimitive null ]
]

{ #category : 'visiting' }
DRInitializePragma >> sccpEvaluateFor: sccp [

	"Nothing to do"
]

{ #category : 'accessing' }
DRInitializePragma >> scope [

	^ scope
]

{ #category : 'accessing' }
DRInitializePragma >> scope: anObject [

	scope := anObject
]
