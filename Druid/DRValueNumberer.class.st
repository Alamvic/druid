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

{ #category : #operations }
DRValueNumberer >> classOf: instruction [

	^ valueNumberList add: (OrderedCollection new
			   add: instruction;
			   yourself)
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
	" group expressions into different sub lists in the value number list.
	 If an expression do not find a belonged sublist then creat a new sublist. "

	valueNumberList size < 1 ifTrue: [
		^ valueNumberList indexOf: (self classOf: instruction) ].
	valueNumberList do: [ :vn |
		vn
			detect: [ :v | self hasSameExpression: instruction as: v ]
			ifFound: [
				vn add: instruction.
				^ valueNumberList indexOf: vn ]
			ifNone: [ ^ valueNumberList indexOf: (self classOf: instruction) ] ]
]
