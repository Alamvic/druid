Class {
	#name : 'DRLoadLiteralVariable',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLoadLiteralVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadLiteralVariable: self
]

{ #category : 'printing' }
DRLoadLiteralVariable >> opcode [
	
	^ 'LoadLit'
]

{ #category : 'SCCP' }
DRLoadLiteralVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]
