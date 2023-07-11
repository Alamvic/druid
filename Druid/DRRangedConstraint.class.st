Class {
	#name : #DRRangedConstraint,
	#superclass : #DRSimpleConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #math }
DRRangedConstraint >> bitAndConstraint: anInteger [
	"Nothing to do"

	^ nil
]

{ #category : #testing }
DRRangedConstraint >> hasAllValuesWithMask: aMask [

	^ false "always?"
]

{ #category : #testing }
DRRangedConstraint >> hasAllValuesWithoutMask: aMask [

	^ false "always?"
]

{ #category : #reversing }
DRRangedConstraint >> reversed [
	
	self subclassResponsibility
]

{ #category : #math }
DRRangedConstraint >> rightShiftConstraint: anInteger [
	"Nothing to do"

	^ nil
]
