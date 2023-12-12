Class {
	#name : #DRLoadStackValue,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadStackValue >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadStackValue: self
]

{ #category : #printing }
DRLoadStackValue >> opcode [
	
	^ 'Load-stack-value'
]

{ #category : #'cogit-generation' }
DRLoadStackValue >> rtlOperandQualifier [

	^ result rtlOperandQualifier
]

{ #category : #SCCP }
DRLoadStackValue >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]
