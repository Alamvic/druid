Class {
	#name : #DRSmallIntegerSpecialisedPrimitive,
	#superclass : #DRSpecialisedPrimitive,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRSmallIntegerSpecialisedPrimitive class >> pragmaName [

	^ #smallInteger
]

{ #category : #api }
DRSmallIntegerSpecialisedPrimitive >> generateSpecialisedPreamble: aGeneratorMethodBuilder [

	aGeneratorMethodBuilder addStatement: self preamble
]

{ #category : #'generation-IR' }
DRSmallIntegerSpecialisedPrimitive >> initialConstraint [

	^ DRRegisterDomain
		  equals: (DRPhysicalRegister name: 'ReceiverResultReg')
		  than: 1
]

{ #category : #api }
DRSmallIntegerSpecialisedPrimitive >> optimize: anIR [

	anIR initialConstraint: self initialConstraint
]

{ #category : #'code-generation' }
DRSmallIntegerSpecialisedPrimitive >> preamble [

	| preamble |
	preamble := DruidTestRTLCompiler lookupSelector: #smallIntegerSpecialisedPreamble.
	^ preamble ast body copy
]
