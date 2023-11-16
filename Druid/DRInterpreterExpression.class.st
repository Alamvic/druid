Class {
	#name : #DRInterpreterExpression,
	#superclass : #DRInterpreterReference,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRInterpreterExpression >> acceptVisitor: aVisitor [

	^ aVisitor visitInterpreterExpression:  self
]

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
