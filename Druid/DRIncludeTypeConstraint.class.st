Class {
	#name : 'DRIncludeTypeConstraint',
	#superclass : 'DRTypeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'testing' }
DRIncludeTypeConstraint >> hasNotType: aDRType [

	^ false
]

{ #category : 'testing' }
DRIncludeTypeConstraint >> hasType: aDRType [

	^ type = aDRType
]

{ #category : 'testing' }
DRIncludeTypeConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasType: type
]

{ #category : 'arithmetic' }
DRIncludeTypeConstraint >> negated [

	^ DRExcludeTypeConstraint withType: type
]

{ #category : 'printing' }
DRIncludeTypeConstraint >> opCode [

	^ 'TYPEOF'
]
