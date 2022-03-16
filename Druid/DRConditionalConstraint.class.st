Class {
	#name : #DRConditionalConstraint,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #initialization }
DRConditionalConstraint class >> withValue: aConstantValue [

	^ self new
		  constantValue: aConstantValue;
		  yourself
]

{ #category : #testing }
DRConditionalConstraint >> includes: otherConstraint [

	^ otherConstraint isDead
]

{ #category : #arithmetic }
DRConditionalConstraint >> includesValue: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRConditionalConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRConditionalConstraint >> includesValuesLessThan: otherConstantValue [

	^ true
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

	^ self
]

{ #category : #printing }
DRConditionalConstraint >> opCode [

	self subclassResponsibility
]

{ #category : #printing }
DRConditionalConstraint >> printOn: aStream [

	aStream nextPutAll: self opCode
]

{ #category : #'as yet unclassified' }
DRConditionalConstraint >> unifyWith: otherConditionalConstraint [

	(self includes: otherConditionalConstraint) ifTrue: [ 
		^ otherConditionalConstraint ].
	(otherConditionalConstraint includes: self) ifTrue: [ 
		^ self ].
	(otherConditionalConstraint negated includes: self) ifTrue: [ 
		^ DRDeadConstraint new ].
	
	^ DRIntersectionConstraint constraints: { self. otherConditionalConstraint }
]
