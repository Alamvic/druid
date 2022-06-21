Class {
	#name : #DRFalseObject,
	#superclass : #DRObjectReferenceValue,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRFalseObject >> acceptVisitor: aVisitor [

	^ aVisitor visitFalseObject: self
]

{ #category : #accessing }
DRFalseObject >> name [
	
	^ 'objectMemory falseObject'
]

{ #category : #accessing }
DRFalseObject >> value [

	^ false
]
