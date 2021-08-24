Class {
	#name : #DRDeadCodeElimination,
	#superclass : #Object,
	#instVars : [
		'instancecfg'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRDeadCodeElimination >> applyTo: cfg [

	instancecfg := cfg. 
	cfg blocksDo: [ :b | self optimiseBlock: b ]. 
	
]

{ #category : #operations }
DRDeadCodeElimination >> dependenciesOf: anInstruction [
	
	| ret |

	anInstruction isConditionalJump ifTrue: [
		self value = self top ifTrue: [ 
			ret := OrderedCollection new. 
			ret addAll: anInstruction trueBranch instructions.
			ret addAll: anInstruction falseBranch instructions. 
			^ ret ]
		ifFalse: [ 
			"We assume self value = self bottom" 
			anInstruction condition value ifTrue: [ 
				 ret := OrderedCollection new. 
				 ret addAll: anInstruction trueBranch instructions.
				^ ret. ]
			ifFalse: [ 
				ret := OrderedCollection new. 
				ret addAll: anInstruction falseBranch instructions.
				^ ret. ] ] ]. 
	^ instancecfg instructions select: [ :instruction | 
		  instruction operands includes: anInstruction ]
]

{ #category : #operations }
DRDeadCodeElimination >> optimiseBlock: aBlock [ 

	self removeDuplicatedInstructions: aBlock. 
	self removeDeadEndInstructions: aBlock. 
	
]

{ #category : #operations }
DRDeadCodeElimination >> removeDeadEndInstructions: aBlock [

	| dependencies removeList|

	removeList := OrderedCollection new. 
	aBlock instructionsDo: [ :i | 
		((i isLoad or: i isStore) or: i isNoop) ifFalse: [
			dependencies := self dependenciesOf: i. 
			dependencies isEmpty ifTrue: [ 
				 removeList add: i.
				 i operands do: [:o | o isConstant ifFalse: [removeList add: o]]]]]. 
	removeList do: [ :i | aBlock removeInstruction: i ]
]

{ #category : #operations }
DRDeadCodeElimination >> removeDuplicatedInstructions: aBlock [

	|instructionList currentInstruction|
	
	instructionList := aBlock instructions. 
	
	[instructionList isEmpty] whileFalse: [ 
		 currentInstruction := instructionList removeFirst. 
		 instructionList do: [:i | 
			currentInstruction = i ifTrue: [
				aBlock removeInstruction: i]]]. 
]
