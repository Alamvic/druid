Class {
	#name : #DRInterpreterExpression,
	#superclass : #DRInterpreterReference,
	#category : #'Druid-IR'
}

{ #category : #'cogit-generation' }
DRInterpreterExpression >> rtlOperandQualifierForInstruction: instruction [

	^ 'Cq:'
]
