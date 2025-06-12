Class {
	#name : 'DRBlockReturn',
	#superclass : 'DRReturn',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'printing' }
DRBlockReturn >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBlockReturn: self
]

{ #category : 'printing' }
DRBlockReturn >> opcode [
	
	^ 'BlockReturn'
]
