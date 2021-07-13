Class {
	#name : #DRSemilattice,
	#superclass : #Object,
	#instVars : [
		'SSAList',
		'SSAValueList'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #initialization }
DRSemilattice >> add: SSAName [
	SSAList add: SSAName. 
]

{ #category : #initialization }
DRSemilattice >> add: ssaname withValue: value [
	SSAList add: ssaname. 
	SSAValueList add: value. 
]

{ #category : #initialization }
DRSemilattice >> check: ssaName1 meets: ssaName2 [

	|bssa1 bssa2|
	
	bssa1 := SSAList indexOf: ssaName1. 
	bssa2 := SSAList indexOf: ssaName2. 
	
	"If the object is not in the Semilattice structure. This case should not happend. "
	bssa1 = 0 ifTrue: [ ^ false ].
	bssa2 = 0 ifTrue: [ ^ false ].
	
	bssa1 > bssa2 ifTrue: [ ^ ssaName2 ]
						ifFalse: [ ^ ssaName1 ].
	
	
]

{ #category : #initialization }
DRSemilattice >> index: ssaName1 [

	|bssa|
	
	bssa := SSAList indexOf: ssaName1. 
	
	"If the object is not in the Semilattice structure. This case should not happend. "
	bssa = 0 ifTrue: [ ^ false ].

	^ bssa
	
]

{ #category : #initialization }
DRSemilattice >> initialize [
	SSAList := OrderedCollection. 
	SSAValueList := OrderedCollection. 
]

{ #category : #initialization }
DRSemilattice >> initialize: anOrderedCollection [

	self assert: anOrderedCollection type = OrderedCollection. 
	SSAList := anOrderedCollection. 
]

{ #category : #initialization }
DRSemilattice >> valueAt: index [
	^ SSAValueList at: index 
]
