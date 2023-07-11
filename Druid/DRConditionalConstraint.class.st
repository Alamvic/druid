Class {
	#name : #DRConditionalConstraint,
	#superclass : #Object,
	#category : #'Druid-IR-Paths'
}

{ #category : #math }
DRConditionalConstraint >> bitAndConstraint: anInteger [
	"Not implemented"

	^ nil
]

{ #category : #arithmetic }
DRConditionalConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalConstraint >> hasAllValuesLessThan: otherConstantValue [

	self subclassResponsibility
]

{ #category : #testing }
DRConditionalConstraint >> hasAllValuesWithMask: aMask [

	self subclassResponsibility
]

{ #category : #testing }
DRConditionalConstraint >> hasAllValuesWithoutMask: aMask [

	self subclassResponsibility
]

{ #category : #testing }
DRConditionalConstraint >> hasNotType: aDRType [

	^ self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalConstraint >> hasObjectValue: objectValue [

	^ false
]

{ #category : #testing }
DRConditionalConstraint >> hasType: aDRType [

	^ self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalConstraint >> hasValue: otherConstantValue [

	self subclassResponsibility
]

{ #category : #testing }
DRConditionalConstraint >> includes: otherConstraint [

	self subclassResponsibility
]

{ #category : #merging }
DRConditionalConstraint >> intersectionWith: otherConditionalConstraint [

	(otherConditionalConstraint includes: self) ifTrue: [ 
		^ self ].
	(self includes: otherConditionalConstraint) ifTrue: [ 
		^ otherConditionalConstraint ].
	(otherConditionalConstraint negated includes: self) ifTrue: [ 
		^ DREmptyConstraint new ].
	
	^ DRIntersectionConstraint constraints: { self. otherConditionalConstraint }
]

{ #category : #testing }
DRConditionalConstraint >> isAny [

	^ false
]

{ #category : #testing }
DRConditionalConstraint >> isEmpty [

	^ false
]

{ #category : #testing }
DRConditionalConstraint >> isUnion [

	^ false
]

{ #category : #constraints }
DRConditionalConstraint >> loadConstraint: anInteger [
	"Nothing to do"

	^ nil
]

{ #category : #arithmetic }
DRConditionalConstraint >> negated [

	self subclassResponsibility
]

{ #category : #printing }
DRConditionalConstraint >> opCode [

	self subclassResponsibility
]

{ #category : #printing }
DRConditionalConstraint >> printOn: aStream [

	aStream nextPutAll: self opCode
]

{ #category : #math }
DRConditionalConstraint >> rightShiftConstraint: anInteger [
	"Not implemented"

	^ nil
]
