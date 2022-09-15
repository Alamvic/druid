Class {
	#name : #DRInline,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRInline >> doApply: cfg [

	cfg messageSends do: [ :i | i inline ]
]
