Class {
	#name : #DRValueNumberer,
	#superclass : #DROptimisation,
	#instVars : [
		'valueNumberList'
	],
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRValueNumberer >> initialize [ 
super initialize.

valueNumberList := OrderedCollection new.
	
]

{ #category : #operations }
DRValueNumberer >> valueNumberOf: instruction [
	"valueNumberList do: [ :vn |
		vn
			detect: instruction
			ifFound: [
				vn add: instruction.
				^ valueNumberList indexOf: vn ]
			ifNone: [
				| newVn |
				valueNumberList add:
					(newVn := OrderedCollection new add: instruction).
				^ valueNumberList indexOf: newVn ] ]"

	valueNumberList size < 1 ifTrue: [
		^ valueNumberList indexOf:
			  (valueNumberList add: (OrderedCollection new
					    add: instruction;
					    yourself)) ].
	valueNumberList do: [ :vn |
		vn
			detect: [ :v | instruction hasSameExpressionAs: v ]
			ifFound: [
				vn add: instruction.
				^ valueNumberList indexOf: vn ]
			ifNone: [
				^ valueNumberList indexOf:
					 (valueNumberList add: (OrderedCollection new
					    add: instruction;
					    yourself)) ] ]
]
