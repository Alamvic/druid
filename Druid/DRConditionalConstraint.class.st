Class {
	#name : #DRConditionalConstraint,
	#superclass : #Object,
	#instVars : [
		'constantValue'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #initialization }
DRConditionalConstraint class >> withValue: aConstantValue [

	^ self new
		  constantValue: aConstantValue;
		  yourself
]

{ #category : #accessing }
DRConditionalConstraint >> constantValue [

	^ constantValue
]

{ #category : #accessing }
DRConditionalConstraint >> constantValue: anObject [

	constantValue := anObject
]

{ #category : #arithmetic }
DRConditionalConstraint >> includes: otherConstraint [

	^ otherConstraint isIncludedIn: self.
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
DRConditionalConstraint >> isIncludedIn: otherConstraint [

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

	aStream
		nextPutAll: self opCode;
		nextPutAll: ' ';
		print: constantValue
]

{ #category : #'as yet unclassified' }
DRConditionalConstraint >> unifyWith: otherConditionalConstraint [

	(self includes: otherConditionalConstraint) ifTrue: [ 
		^ otherConditionalConstraint ].
	(otherConditionalConstraint includes: self) ifTrue: [ 
		^ self ].
	
	^ DRIntersectionConstraint constraints: { self. otherConditionalConstraint }
]
