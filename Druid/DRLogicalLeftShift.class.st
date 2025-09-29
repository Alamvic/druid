Class {
	#name : 'DRLogicalLeftShift',
	#superclass : 'DRArithmeticInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLogicalLeftShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLeftShift: self
]

{ #category : 'SCCP' }
DRLogicalLeftShift >> sccpOperateOnLattice: anInteger and: anInteger2 [ 
	
	^ anInteger << anInteger2
]

{ #category : 'types' }
DRLogicalLeftShift >> typeWithAlreadySeen: aCollection [

	^ DRUnsignedIntegerType bits64
]
