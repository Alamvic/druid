Class {
	#name : #DRSimpleConstraint,
	#superclass : #DRConditionalConstraint,
	#instVars : [
		'constantValue'
	],
	#category : #'Druid-IR-Paths'
}

{ #category : #initialization }
DRSimpleConstraint class >> withValue: aConstantValue [

	^ self new
		  constantValue: aConstantValue value;
		  yourself
]

{ #category : #comparing }
DRSimpleConstraint >> = otherConstraint [

	^ self class = otherConstraint class and: [ 
		  self constantValue = otherConstraint constantValue ]
]

{ #category : #accessing }
DRSimpleConstraint >> constantValue [

	^ constantValue
]

{ #category : #accessing }
DRSimpleConstraint >> constantValue: anObject [

	constantValue := anObject
]

{ #category : #comparing }
DRSimpleConstraint >> hash [
	"Hash implemented because #= was implemented"

	^ self class hash bitXor: self constantValue hash
]

{ #category : #arithmetic }
DRSimpleConstraint >> includes: otherConstraint [

	^ self negated isIncludedIn: otherConstraint negated.
]

{ #category : #arithmetic }
DRSimpleConstraint >> isIncludedIn: otherConstraint [

	self subclassResponsibility 
]

{ #category : #printing }
DRSimpleConstraint >> printOn: aStream [

	aStream
		nextPutAll: self opCode;
		nextPutAll: ' ';
		print: constantValue
]
