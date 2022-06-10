Class {
	#name : #DRSmallIntegerPrimitiveCustomisation,
	#superclass : #DRPrimitiveCustomisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRSmallIntegerPrimitiveCustomisation class >> pragmaName [

	^ #smallInteger
]

{ #category : #api }
DRSmallIntegerPrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	aGeneratorMethodBuilder addStatement: self preamble
]

{ #category : #'generation-IR' }
DRSmallIntegerPrimitiveCustomisation >> initialConstraint [

	^ DRRegisterDomain
		  equals: (DRPhysicalRegister name: 'ReceiverResultReg')
		  than: 1
]

{ #category : #api }
DRSmallIntegerPrimitiveCustomisation >> optimize: anIR [

	anIR initialConstraint: self initialConstraint
]

{ #category : #'code-generation' }
DRSmallIntegerPrimitiveCustomisation >> preamble [

	| preamble |
	preamble := DruidTestRTLCompiler lookupSelector: #smallIntegerSpecialisedPreamble.
	^ preamble ast body copy
]
