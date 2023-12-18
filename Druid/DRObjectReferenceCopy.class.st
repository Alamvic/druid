Class {
	#name : #DRObjectReferenceCopy,
	#superclass : #DRStrongCopy,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRObjectReferenceCopy >> isObjectReferenceCopy [

	^ true
]

{ #category : #testing }
DRObjectReferenceCopy >> shouldKeepInCFG [

	^ true
]
