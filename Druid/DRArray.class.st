Class {
	#name : 'DRArray',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRArray >> acceptVisitor: aVisitor [
	
	^ aVisitor visitArrayNode: self
]

{ #category : 'visiting' }
DRArray >> opcode [
	
	^ 'Array'
]

{ #category : 'visiting' }
DRArray >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]
