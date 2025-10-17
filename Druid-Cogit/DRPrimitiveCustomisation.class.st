Class {
	#name : 'DRPrimitiveCustomisation',
	#superclass : 'Object',
	#category : 'Druid-Cogit-Customization',
	#package : 'Druid-Cogit',
	#tag : 'Customization'
}

{ #category : 'instance creation' }
DRPrimitiveCustomisation class >> newCustomisation: specialisedSelector [

	| specialisedClass |
	specialisedClass := self subclasses detect: [ :class | 
		                    class pragmaName = specialisedSelector ].
	^ specialisedClass new
]

{ #category : 'api' }
DRPrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	self subclassResponsibility
]

{ #category : 'api' }
DRPrimitiveCustomisation >> optimize: anIR [

	self subclassResponsibility
]
