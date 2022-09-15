Class {
	#name : #DRNoOverflow,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRNoOverflow >> isNoMultiplyOverflow [
	
	^ false
]

{ #category : #testing }
DRNoOverflow >> isNoOverflow [

	^ true
]
