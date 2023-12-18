Class {
	#name : #DRInterpreterExpression,
	#superclass : #DRInterpreterReference,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRInterpreterExpression >> acceptVisitor: aVisitor [

	^ aVisitor visitInterpreterExpression:  self
]

{ #category : #testing }
DRInterpreterExpression >> isJITCompileTimeExpression [

	^ true
]

{ #category : #'cogit-generation' }
DRInterpreterExpression >> rtlOperandQualifierForInstruction: instruction [

	^ 'Cq:'
]
