Class {
	#name : 'DREqualsObjectConstraint',
	#superclass : 'DREqualsConstraint',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'testing' }
DREqualsObjectConstraint >> hasObjectValue: objectValue [

	^ constantValue = objectValue
]

{ #category : 'testing' }
DREqualsObjectConstraint >> isIncludedIn: otherConstraint [

	^ otherConstraint hasObjectValue: constantValue
]

{ #category : 'arithmetic' }
DREqualsObjectConstraint >> negated [

	^ DRNotEqualsObjectConstraint new
		  constantValue: constantValue;
		  yourself
]
