Class {
	#name : #DREqualsObjectConstraint,
	#superclass : #DREqualsConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #arithmetic }
DREqualsObjectConstraint >> negated [

	^ DRNotEqualsObjectConstraint withValue: constantValue
]
