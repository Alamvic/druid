Class {
	#name : #DRLeftShift,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLeftShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLeftShift: self
]

{ #category : #printing }
DRLeftShift >> opcode [
	
	^ 'LeftShift'
]

{ #category : #SCCP }
DRLeftShift >> sccpOperateOnLattice: anInteger and: anInteger2 [ 
	
	^ anInteger << anInteger2
]
