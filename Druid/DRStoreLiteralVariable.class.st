Class {
	#name : 'DRStoreLiteralVariable',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRStoreLiteralVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitStoreLiteralVariable: self
]

{ #category : 'visiting' }
DRStoreLiteralVariable >> isMandatoryInstruction [ 

	^ true
]

{ #category : 'visiting' }
DRStoreLiteralVariable >> isStore [ 

	^ true
]

{ #category : 'printing' }
DRStoreLiteralVariable >> opcode [
	
	^ 'StoreLit'
]

{ #category : 'SCCP' }
DRStoreLiteralVariable >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
