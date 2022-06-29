Class {
	#name : #DRTrueObject,
	#superclass : #DRObjectReferenceValue,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRTrueObject >> acceptVisitor: aVisitor [

	^ aVisitor visitTrueObject: self
]

{ #category : #accessing }
DRTrueObject >> name [
	
	^ 'objectMemory trueObject'
]

{ #category : #accessing }
DRTrueObject >> value [

	^ true
]
