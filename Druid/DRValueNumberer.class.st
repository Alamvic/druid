Class {
	#name : #DRValueNumberer,
	#superclass : #DRSCCP,
	#instVars : [
		'valueNumberList'
	],
	#category : #'Druid-Tests'
}

{ #category : #executing }
DRValueNumberer >> applyPropagationToInstruction: i [
"improve this inheritance"
]

{ #category : #testing }
DRValueNumberer >> hasSameExpression: anIntruction as: otherIntruction [

	^ (self latticeValue: anIntruction) = (self latticeValue: otherIntruction) 
]

{ #category : #initialization }
DRValueNumberer >> initialize [ 
super initialize.

valueNumberList := OrderedCollection new.
	
]

{ #category : #operations }
DRValueNumberer >> valueNumberOf: instruction [

	valueNumberList size < 1 ifTrue: [
		^ valueNumberList indexOf:
			  (valueNumberList add: (OrderedCollection new
					    add: instruction;
					    yourself)) ].
	valueNumberList do: [ :vn |
		vn
			detect: [ :v | self hasSameExpression: instruction as: v ]
			ifFound: [
				vn add: instruction.
				^ valueNumberList indexOf: vn ]
			ifNone: [
				^ valueNumberList indexOf:
					 (valueNumberList add: (OrderedCollection new
					    add: instruction;
					    yourself)) ] ]
]
