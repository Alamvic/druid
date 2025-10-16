Class {
	#name : 'DRNonePrimitiveCustomisation',
	#superclass : 'DRPrimitiveCustomisation',
	#category : 'Druid-Cogit-Customization',
	#package : 'Druid-Cogit',
	#tag : 'Customization'
}

{ #category : 'as yet unclassified' }
DRNonePrimitiveCustomisation class >> pragmaName [

	^ #none
]

{ #category : 'api' }
DRNonePrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	"Do nothing"
]

{ #category : 'api' }
DRNonePrimitiveCustomisation >> optimize: anIR [

	"Do nothing"
]
