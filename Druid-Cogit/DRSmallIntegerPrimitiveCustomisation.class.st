Class {
	#name : 'DRSmallIntegerPrimitiveCustomisation',
	#superclass : 'DRPrimitiveCustomisation',
	#category : 'Druid-Cogit-Customization',
	#package : 'Druid-Cogit',
	#tag : 'Customization'
}

{ #category : 'accessing' }
DRSmallIntegerPrimitiveCustomisation class >> pragmaName [

	^ #smallInteger
]

{ #category : 'api' }
DRSmallIntegerPrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	aGeneratorMethodBuilder addStatement: self preamble
]

{ #category : 'generation-IR' }
DRSmallIntegerPrimitiveCustomisation >> initialConstraint [

	^ DRReceiverConstraint with: (DRAnyMaskConstraint withValue: 1)
]

{ #category : 'api' }
DRSmallIntegerPrimitiveCustomisation >> optimize: anIR [

	anIR initialConstraint: self initialConstraint
]

{ #category : 'code-generation' }
DRSmallIntegerPrimitiveCustomisation >> preamble [

	| preamble |
	preamble := DruidJIT lookupSelector: #smallIntegerSpecialisedPreamble.
	^ preamble ast body copy
]
