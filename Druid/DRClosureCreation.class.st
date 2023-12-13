Class {
	#name : #DRClosureCreation,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRClosureCreation >> acceptVisitor: aVisitor [

	^ aVisitor visitClosureCreation: self
]

{ #category : #testing }
DRClosureCreation >> isMandatoryInstruction [

	^ true
]

{ #category : #'cogit-generation' }
DRClosureCreation >> rtlOperandQualifier [

	^ result rtlOperandQualifier
]

{ #category : #SCCP }
DRClosureCreation >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
