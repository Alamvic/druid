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
DRRegisterConstraint class >> greater: register than: aValue [

	^ self new
		  reg: register;
		  condition: (DRGreaterConstraint withValue: aValue);
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
DRRegisterConstraint class >> lessOrEquals: register than: aValue [

	^ self new
		  reg: register;
		  condition: (DRLessOrEqualsConstraint withValue: aValue);
		  yourself
]

{ #category : #'as yet unclassified' }
DRRegisterConstraint class >> mask: register with: aValue [

	^ self new
		  reg: register;
		  condition: (DRMaskConstraint withValue: aValue);
		  yourself
]

{ #category : #'as yet unclassified' }
DRRegisterConstraint class >> notEquals: register than: aValue [

	^ self new
		  reg: register;
		  condition: (DRNotEqualsConstraint withValue: aValue);
		  yourself
]

{ #category : #'as yet unclassified' }
DRRegisterConstraint class >> reg: register constraint: condition [

	^ self new
		  reg: register;
		  condition: condition;
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

{ #category : #operations }
DRRegisterConstraint >> mergeWith: aDRRegisterConstraint [ 
	
	^ self class new
		  reg: reg;
		  condition: (condition unifyWith: aDRRegisterConstraint condition);
		  yourself  
]

{ #category : #arithmetic }
DRRegisterConstraint >> negated [

	^ self class new
		  reg: reg;
		  condition: condition negated;
		  yourself
]

{ #category : #printing }
DRRegisterConstraint >> printOn: aStream [

	aStream 
		nextPutAll: reg name;
		nextPutAll: ' ';
		print: condition
]

{ #category : #accessing }
DRRegisterConstraint >> reg [

	^ reg
]

{ #category : #accessing }
DRRegisterConstraint >> reg: anObject [

	reg := anObject
]
