Class {
	#name : 'DRLoadFramePointer',
	#superclass : 'DRAbstractLoad',
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLoadFramePointer >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadFramePointer: self
]

{ #category : 'testing' }
DRLoadFramePointer >> isLoadFramePointer [
	
	^ true
]

{ #category : 'printing' }
DRLoadFramePointer >> opcode [
	
	^ 'LoadFramePointer'
]

{ #category : 'SCCP' }
DRLoadFramePointer >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]

{ #category : 'types' }
DRLoadFramePointer >> typeWithAlreadySeen: aCollection [

	^ DRPointerType new
]
