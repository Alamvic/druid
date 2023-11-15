Class {
	#name : #DRAbstractLoad,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #types }
DRAbstractLoad >> type [

	^ self subclassResponsibility
]
