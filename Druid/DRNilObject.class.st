Class {
	#name : #DRNilObject,
	#superclass : #DRObjectReferenceValue,
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRNilObject >> name [
	
	^ 'objectMemory nilObject'
]

{ #category : #accessing }
DRNilObject >> value [

	^ nil
]
