Class {
	#name : #DRRightShift,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRRightShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitRightShift: self
]

{ #category : #printing }
DRRightShift >> opcode [
	
	^ 'RightShift'
]

{ #category : #SCCP }
DRRightShift >> sccpOperateOnLattice: anInteger and: anInteger2 [ 
	
	^ anInteger >> anInteger2
]

{ #category : #types }
DRRightShift >> type [
	
	^ DRIntegerType new
]
