Class {
	#name : #DRDeadEdgeSplitting,
	#superclass : #DRPathOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadEdgeSplitting >> doApplyWithGeneratedPaths: aCFG [

	"Find first (longest) dead path in CFG and split guilty edge"

	| edge |
	aCFG deadPaths ifEmpty: [ ^ self ].
	
	edge := (aCFG deadPaths associations detectMax: [ :v | v value size ]) key.
	[ edge destination predecessors size = 1 ] whileTrue: [ 
		edge := edge parent.
		edge ifNil: [ ^ self ] ].

	('Splitting: ' , edge destination id asString , ' -> ' , edge source id asString) traceCr.

	edge split

]
