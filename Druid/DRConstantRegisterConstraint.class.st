Class {
	#name : #DRConstantRegisterConstraint,
	#superclass : #Object,
	#instVars : [
		'constant'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #'instance creation' }
DRConstantRegisterConstraint class >> new: aValue [

	^ self new
		  constant: aValue;
		  yourself
]

{ #category : #accessing }
DRConstantRegisterConstraint >> constant [

	^ constant
]

{ #category : #accessing }
DRConstantRegisterConstraint >> constant: anObject [

	constant := anObject
]
