Class {
	#name : #DRSignedRightShift,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRSignedRightShift >> acceptVisitor: aVisitor [

	^ aVisitor visitSignedRightShift: self
]

{ #category : #SCCP }
DRSignedRightShift >> sccpOperateOnLattice: anInteger and: anInteger2 [ 
	
	^ anInteger >> anInteger2
]
