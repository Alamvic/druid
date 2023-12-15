Class {
	#name : #DRRegisterDomain,
	#superclass : #Object,
	#instVars : [
		'reg',
		'constraint'
	],
	#category : #'Druid-IR-Paths'
}

{ #category : #factory }
DRRegisterDomain class >> equals: register than: aValue [

	^ self
		  reg: register
		  constraint: (DREqualsConstraint withValue: aValue)
]

{ #category : #factory }
DRRegisterDomain class >> greater: register than: aValue [

	^ self
		  reg: register
		  constraint: (DRGreaterConstraint withValue: aValue)
]

{ #category : #factory }
DRRegisterDomain class >> greaterOrEquals: register than: aValue [

	^ self
		  reg: register
		  constraint: (DRGreaterOrEqualsConstraint withValue: aValue)
]

{ #category : #factory }
DRRegisterDomain class >> less: register than: aValue [

	^ self reg: register constraint: (DRLessConstraint withValue: aValue)
]

{ #category : #factory }
DRRegisterDomain class >> lessOrEquals: register than: aValue [

	^ self
		  reg: register
		  constraint: (DRLessOrEqualsConstraint withValue: aValue)
]

{ #category : #factory }
DRRegisterDomain class >> mask: register with: aValue [

	^ self reg: register constraint: (DRAnyMaskConstraint withValue: aValue)
]

{ #category : #factory }
DRRegisterDomain class >> notEquals: register than: aValue [

	^ self
		  reg: register
		  constraint: (DRNotEqualsConstraint withValue: aValue)
]

{ #category : #factory }
DRRegisterDomain class >> reg: register constraint: aConstraint [

	^ self new
		  reg: register;
		  constraint: aConstraint;
		  yourself
]

{ #category : #factory }
DRRegisterDomain class >> type: register with: aDRType [

	^ self
		  reg: register
		  constraint: (DRIncludeTypeConstraint withType: aDRType)
]

{ #category : #adding }
DRRegisterDomain >> addToEdge: anEdge [

	anEdge addConstraint: self constraint to: self reg name
]

{ #category : #accessing }
DRRegisterDomain >> constraint [

	^ constraint
]

{ #category : #accessing }
DRRegisterDomain >> constraint: anObject [

	constraint := anObject
]

{ #category : #testing }
DRRegisterDomain >> includes: otherRegisterConstraint [

	^ reg = otherRegisterConstraint reg and: [ constraint includes: otherRegisterConstraint constraint ]
]

{ #category : #testing }
DRRegisterDomain >> isDead [

	^ constraint isEmpty
]

{ #category : #operations }
DRRegisterDomain >> mergedWith: aDRRegisterConstraint [ 
	
	^ self class
		  reg: reg
		  constraint: (constraint intersectionWith: aDRRegisterConstraint constraint)
]

{ #category : #arithmetic }
DRRegisterDomain >> negated [

	^ self class new
		  reg: reg;
		  constraint: constraint negated;
		  yourself
]

{ #category : #printing }
DRRegisterDomain >> printOn: aStream [

	aStream 
		nextPutAll: reg name;
		nextPutAll: ' ';
		print: constraint
]

{ #category : #accessing }
DRRegisterDomain >> reg [

	^ reg
]

{ #category : #accessing }
DRRegisterDomain >> reg: anObject [

	reg := anObject
]

{ #category : #reversing }
DRRegisterDomain >> reversed [

	^ self class new
		  reg: reg;
		  constraint: constraint reversed;
		  yourself
]
