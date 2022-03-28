Class {
	#name : #DRConditionalConstraint,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRConditionalConstraint >> includes: otherConstraint [

	self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalConstraint >> includesValue: otherConstantValue [

	self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalConstraint >> includesValuesGreaterThan: otherConstantValue [

	self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalConstraint >> includesValuesLessThan: otherConstantValue [

	self subclassResponsibility
]

{ #category : #testing }
DRConditionalConstraint >> isDead [

	^ false
]

{ #category : #testing }
DRConditionalConstraint >> isUnion [

	^ false
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

{ #category : #merging }
DRConditionalConstraint >> unifyWith: otherConditionalConstraint [

	(self includes: otherConditionalConstraint) ifTrue: [ 
		^ otherConditionalConstraint ].
	(otherConditionalConstraint includes: self) ifTrue: [ 
		^ self ].
	(otherConditionalConstraint negated includes: self) ifTrue: [ 
		^ DREmptyConstraint new ].
	
	^ DRIntersectionConstraint constraints: { self. otherConditionalConstraint }
]
