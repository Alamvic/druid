Class {
	#name : #DRPrimitiveSpecialisation,
	#superclass : #Object,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'instance creation' }
DRPrimitiveSpecialisation class >> newCustomisation: specialisedSelector [

	| specialisedClass |
	specialisedClass := self subclasses detect: [ :class | 
		                    class pragmaName = specialisedSelector ].
	^ specialisedClass new
]

{ #category : #api }
DRPrimitiveSpecialisation >> addInitialConstraint: anIR [

	self subclassResponsibility
]

{ #category : #api }
DRPrimitiveSpecialisation >> specialiseIR: aDRMethodIRGenerator [
	
	self subclassResponsibility 

	
]
