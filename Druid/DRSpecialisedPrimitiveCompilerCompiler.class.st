Class {
	#name : #DRSpecialisedPrimitiveCompilerCompiler,
	#superclass : #DRPrimitiveCompilerCompiler,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'generation-IR' }
DRSpecialisedPrimitiveCompilerCompiler >> generateDruidIRFor: method [

	| ir |
	ir := super generateDruidIRFor: method.
	ir initialConstraint: self initialConstraint.
	^ ir
]

{ #category : #'generation-IR' }
DRSpecialisedPrimitiveCompilerCompiler >> initialConstraint [

	^ DRRegisterDomain
		  equals: (DRPhysicalRegister name: 'ReceiverResultReg')
		  than: 1
]

{ #category : #'generation-IR' }
DRSpecialisedPrimitiveCompilerCompiler >> initialize [

	super initialize.
	codeGenerator := DRCogitSpecialisedCodeGenerator new
]
