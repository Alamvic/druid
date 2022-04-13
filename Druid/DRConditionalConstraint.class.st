Class {
	#name : #DRConditionalConstraint,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

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

	(self includes: otherConditionalConstraint) ifTrue: [ 
		^ otherConditionalConstraint ].
	(otherConditionalConstraint includes: self) ifTrue: [ 
		^ self ].
	(otherConditionalConstraint negated includes: self) ifTrue: [ 
		^ DREmptyConstraint new ].
	
	^ DRIntersectionConstraint constraints: { self. otherConditionalConstraint }
]

{ #category : #testing }
DRConditionalConstraint >> isEmpty [

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
