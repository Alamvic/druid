Class {
	#name : #DRCogitBytecodeCodeGenerator,
	#superclass : #DRCogitCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitBytecodeCodeGenerator >> visitCall: aDRCall [

	super visitCall: aDRCall.
	aDRCall isMapped ifTrue: [
		DRAnnotateBytecode new acceptVisitor: self.
		
		"Validate only one annotateBytecode: in the path. And the annotation in the table."
		self flag: #TODO. "See DRMethod >> mustAnnotateBytecode" ]
]
