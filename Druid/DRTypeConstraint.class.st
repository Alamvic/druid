Class {
	#name : #DRTypeConstraint,
	#superclass : #DRConditionalConstraint,
	#instVars : [
		'type'
	],
	#category : #'Druid-IR-Paths'
}

{ #category : #accessing }
DRTypeConstraint class >> withType: types [

	^ self new
		  type: types;
		  yourself
]

{ #category : #arithmetic }
DRTypeConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ false
]

{ #category : #arithmetic }
DRTypeConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ false
]

{ #category : #testing }
DRTypeConstraint >> hasAllValuesWithMask: aMask [

	^ false
]

{ #category : #testing }
DRTypeConstraint >> hasAllValuesWithoutMask: aMask [

	^ false
]

{ #category : #arithmetic }
DRTypeConstraint >> hasValue: otherConstantValue [
	"Only for constants values"

	^ false
]

{ #category : #testing }
DRTypeConstraint >> includes: otherConstraint [

	^ self negated isIncludedIn: otherConstraint negated.
]

{ #category : #testing }
DRTypeConstraint >> isIncludedIn: otherConstraint [

	self subclassResponsibility 
]

{ #category : #printing }
DRTypeConstraint >> printOn: aStream [

	super printOn: aStream.
	aStream << ' '.
	aStream << type name
]

{ #category : #accessing }
DRTypeConstraint >> type [

	^ type
]

{ #category : #accessing }
DRTypeConstraint >> type: anObject [

	type := anObject
]
