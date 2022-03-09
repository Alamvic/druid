Class {
	#name : #DRConditionalRegisterConstraint,
	#superclass : #Object,
	#instVars : [
		'reg',
		'constantValue'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #'as yet unclassified' }
DRConditionalRegisterConstraint class >> equals: register than: aValue [

	^ DREqualsRegisterConstraint new
		  reg: register;
		  constantValue: aValue;
		  yourself
]

{ #category : #'as yet unclassified' }
DRConditionalRegisterConstraint class >> greaterOrEquals: register than: aValue [

	^ DRGreaterOrEqualsRegisterConstraint new
		  reg: register;
		  constantValue: aValue;
		  yourself
]

{ #category : #accessing }
DRConditionalRegisterConstraint >> constantValue [

	^ constantValue
]

{ #category : #accessing }
DRConditionalRegisterConstraint >> constantValue: anObject [

	constantValue := anObject
]

{ #category : #arithmetic }
DRConditionalRegisterConstraint >> includes: otherConstraint [

	self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalRegisterConstraint >> includesValue: aConstantValue [

	self subclassResponsibility
]

{ #category : #arithmetic }
DRConditionalRegisterConstraint >> negated [

	self subclassResponsibility
]

{ #category : #accessing }
DRConditionalRegisterConstraint >> reg [

	^ reg
]

{ #category : #accessing }
DRConditionalRegisterConstraint >> reg: anObject [

	reg := anObject
]
