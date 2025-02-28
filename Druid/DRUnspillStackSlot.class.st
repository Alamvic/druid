Class {
	#name : 'DRUnspillStackSlot',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRUnspillStackSlot >> acceptVisitor: aVisitor [
	
	^ aVisitor visitUnspillStackSlot: self
]

{ #category : 'visiting' }
DRUnspillStackSlot >> isMandatoryInstruction [

	^ true
]

{ #category : 'testing' }
DRUnspillStackSlot >> isUnspill [
	
	^ true
]

{ #category : 'SCCP' }
DRUnspillStackSlot >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]
