Class {
	#name : 'DRExcludeTypeConstraint',
	#superclass : 'DRTypeConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'testing' }
DRExcludeTypeConstraint >> hasNotType: aDRType [

	^ type = aDRType
]

{ #category : 'testing' }
DRExcludeTypeConstraint >> hasType: aDRType [

	^ type ~= aDRType
]

{ #category : 'testing' }
DRExcludeTypeConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasNotType: type
]

{ #category : 'arithmetic' }
DRExcludeTypeConstraint >> negated [

	^ DRIncludeTypeConstraint withType: type
]

{ #category : 'printing' }
DRExcludeTypeConstraint >> opCode [

	^ 'NOTTYPEOF'
]
