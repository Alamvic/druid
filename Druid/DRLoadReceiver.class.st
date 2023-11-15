Class {
	#name : #DRLoadReceiver,
	#superclass : #DRInstruction,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRLoadReceiver >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitLoadReceiver: self
]

{ #category : #testing }
DRLoadReceiver >> isLoadReceiver [
	
	^ true
]

{ #category : #printing }
DRLoadReceiver >> opcode [
	
	^ 'LoadReceiver'
]

{ #category : #SCCP }
DRLoadReceiver >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]

{ #category : #types }
DRLoadReceiver >> type [
	
	^ DRSignedIntegerType new
]
