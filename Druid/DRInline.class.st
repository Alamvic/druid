Class {
	#name : #DRInline,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRInline >> doApply: cfg [

	cfg messageSends copy do: [ :i | i inline ]
]
