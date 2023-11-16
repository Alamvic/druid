Class {
	#name : #DRLoadStackPointer,
	#superclass : #DRAbstractLoad,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadStackPointer >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadStackPointer: self
]

{ #category : #printing }
DRLoadStackPointer >> opcode [
	
	^ 'LoadStackPointer'
]

{ #category : #SCCP }
DRLoadStackPointer >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the stack pointer"
	^ sccp bottom
]

{ #category : #types }
DRLoadStackPointer >> type [

	^ DRPointerType new
]
