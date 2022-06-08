Class {
	#name : #DRSpecialisedPrimitiveCompilerCompiler,
	#superclass : #DRPrimitiveCompilerCompiler,
	#instVars : [
		'specialisation'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRSpecialisedPrimitiveCompilerCompiler >> generateDruidIRFor: method [

	method ast
		pragmaNamed: #specialiseReciverFor:
		ifPresent: [ :pragma | specialisation := self newSpecialisation: pragma arguments first value ].

	^ super generateDruidIRFor: method
]

{ #category : #'generation-IR' }
DRSpecialisedPrimitiveCompilerCompiler >> generateTargetASTFromIR: anIR [

	self specialisation generateSpecialisedPreamble: codeGenerator generatorMethodBuilder .
	^ super generateTargetASTFromIR: anIR
]

{ #category : #specialisation }
DRSpecialisedPrimitiveCompilerCompiler >> newSpecialisation: specialisedSelector [

	| specialisedClass |
	specialisedClass := DRSpecialisedPrimitive subclasses detect: [ 
		                    :class | class pragmaName = specialisedSelector ].
	^ specialisedClass new
]

{ #category : #'generation-IR' }
DRSpecialisedPrimitiveCompilerCompiler >> optimize: anIR [

	self specialisation optimize: anIR.
	super optimize: anIR
]

{ #category : #accessing }
DRSpecialisedPrimitiveCompilerCompiler >> specialisation [
	^ specialisation
]
