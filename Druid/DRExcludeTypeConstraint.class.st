Class {
	#name : #DRExcludeTypeConstraint,
	#superclass : #DRTypeConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #testing }
DRExcludeTypeConstraint >> hasType: aDRType [

	^ type ~= aDRType
]

{ #category : #testing }
DRExcludeTypeConstraint >> includes: otherConstraint [

	^ otherConstraint hasNotType: type
]

{ #category : #arithmetic }
DRExcludeTypeConstraint >> negated [

	^ DRIncludeTypeConstraint withType: type
]

{ #category : #printing }
DRExcludeTypeConstraint >> opCode [

	^ 'NOTTYPEOF'
]
