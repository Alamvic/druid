Class {
	#name : #DRDummyInterpreter,
	#superclass : #DRInterpreterBuilder,
	#category : #'Druid-Tests'
}

{ #category : #'instance creation' }
DRDummyInterpreter >> basicNew [
	"Answer a new <AbstractInterpreter>"

	^ DREmptyInterpreter basicNew
]
