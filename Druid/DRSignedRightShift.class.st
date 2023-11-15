Class {
	#name : #DRSignedRightShift,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRSignedRightShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitRightShift: self
]

{ #category : #SCCP }
DRSignedRightShift >> sccpOperateOnLattice: anInteger and: anInteger2 [ 
	
	^ anInteger >> anInteger2
]
