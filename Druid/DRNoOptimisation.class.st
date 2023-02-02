Class {
	#name : #DRNoOptimisation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRNoOptimisation >> applyTo: cfg [

	"Do nothing"
]

{ #category : #testing }
DRNoOptimisation >> isNoOptimisation [

	^ true
]
