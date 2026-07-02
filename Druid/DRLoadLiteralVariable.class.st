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

{ #category : 'accessing' }
DRLoadLiteralVariable >> literal [

	^ self operand1 value value
]

{ #category : 'printing' }
DRLoadLiteralVariable >> opcode [
	
	^ 'LoadLit'
]

{ #category : 'SCCP' }
DRLoadLiteralVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'types' }
DRLoadLiteralVariable >> type [

	^ self literal class asDRType
]
