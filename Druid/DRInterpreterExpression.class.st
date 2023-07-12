Class {
	#name : #DRInterpreterExpression,
	#superclass : #DRInterpreterReference,
	#category : #'Druid-IR'
}

{ #category : #converting }
DRInterpreterExpression >> asJitCompileTimeExpression [

	^ reference asDRValue asJitCompileTimeExpression
]

{ #category : #testing }
DRInterpreterExpression >> isJITCompileTimeExpression [

	^ true
]

{ #category : #'cogit-generation' }
DRInterpreterExpression >> rtlOperandQualifierForInstruction: instruction [

	^ 'Cq:'
]
