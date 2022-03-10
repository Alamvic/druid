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

	self subclassResponsibility 
]

{ #category : #'as yet unclassified' }
DRConditionalConstraint >> unifyWith: otherConditionalConstraint [

	(self includes: otherConditionalConstraint) ifTrue: [ 
		^ otherConditionalConstraint ].
	(otherConditionalConstraint includes: self) ifTrue: [ 
		^ self ].
	(otherConditionalConstraint negated includes: self) ifTrue: [ 
		^ nil "Dead branch" ].
	
	^ DRIntersectionConstraint constraints: { self. otherConditionalConstraint }
]
