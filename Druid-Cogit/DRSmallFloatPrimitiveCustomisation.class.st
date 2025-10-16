Class {
	#name : 'DRSmallFloatPrimitiveCustomisation',
	#superclass : 'DRPrimitiveCustomisation',
	#category : 'Druid-Cogit-Customization',
	#package : 'Druid-Cogit',
	#tag : 'Customization'
}

{ #category : 'accessing' }
DRSmallFloatPrimitiveCustomisation class >> pragmaName [

	^ #smallFloat
]

{ #category : 'api' }
DRSmallFloatPrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	aGeneratorMethodBuilder addStatement: self preamble
]

{ #category : 'generation-IR' }
DRSmallFloatPrimitiveCustomisation >> initialConstraint [

	^ DRReceiverConstraint with: (DREqualsConstraint withValue: 16r100)
]

{ #category : 'api' }
DRSmallFloatPrimitiveCustomisation >> optimize: anIR [

	anIR initialConstraint: self initialConstraint
]

{ #category : 'code-generation' }
DRSmallFloatPrimitiveCustomisation >> preamble [

	| preamble |
	preamble := DruidJIT lookupSelector: #smallFloatSpecialisedPreamble.
	^ preamble ast body copy
]
