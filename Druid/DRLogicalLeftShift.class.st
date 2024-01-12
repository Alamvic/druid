Class {
	#name : #DRLogicalLeftShift,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLogicalLeftShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLeftShift: self
]

{ #category : #SCCP }
DRLogicalLeftShift >> sccpOperateOnLattice: anInteger and: anInteger2 [ 
	
	^ anInteger << anInteger2
]

{ #category : #types }
DRLogicalLeftShift >> type [

	^ DRUnsignedIntegerType size: 8 "bytes per word"
]
