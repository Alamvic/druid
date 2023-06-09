Class {
	#name : #DRInterpreterExpression,
	#superclass : #DRInterpreterReference,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRInterpreterExpression >> isJITCompileTimeExpression [

	^ true
]

{ #category : #'cogit-generation' }
DRInterpreterExpression >> rtlOperandQualifierForInstruction: instruction [

	^ 'Cq:'
]
