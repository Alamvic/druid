Class {
	#name : #DRRegisterConstraint,
	#superclass : #Object,
	#instVars : [
		'reg',
		'condition'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #'as yet unclassified' }
DRRegisterConstraint class >> equals: register than: aValue [

	^ self new
		  reg: register;
		  condition: (DREqualsConstraint withValue: aValue);
		  yourself
]

{ #category : #'as yet unclassified' }
DRRegisterConstraint class >> greaterOrEquals: register than: aValue [

	^ self new
		  reg: register;
		  condition: (DRGreaterOrEqualsConstraint withValue: aValue);
		  yourself
]

{ #category : #'as yet unclassified' }
DRRegisterConstraint class >> less: register than: aValue [

	^ self new
		  reg: register;
		  condition: (DRLessConstraint withValue: aValue);
		  yourself
]

{ #category : #'as yet unclassified' }
DRRegisterConstraint class >> notEquals: register than: aValue [

	^ self new
		  reg: register;
		  condition: (DRNotEqualsConstraint withValue: aValue);
		  yourself
]

{ #category : #accessing }
DRRegisterConstraint >> condition [

	^ condition
]

{ #category : #accessing }
DRRegisterConstraint >> condition: anObject [

	condition := anObject
]

{ #category : #testing }
DRRegisterConstraint >> includes: otherRegisterConstraint [

	^ reg = otherRegisterConstraint reg and: [ condition includes: otherRegisterConstraint condition ]
]

{ #category : #arithmetic }
DRRegisterConstraint >> negated [

	^ self class new
		  reg: reg;
		  condition: condition negated;
		  yourself
]

{ #category : #accessing }
DRRegisterConstraint >> reg [

	^ reg
]

{ #category : #accessing }
DRRegisterConstraint >> reg: anObject [

	reg := anObject
]
