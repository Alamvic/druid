Class {
	#name : #DRBasicBlock,
	#superclass : #Object,
	#instVars : [
		'id',
		'instructions',
		'blockValue',
		'endInstruction'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRBasicBlock class >> id: anInteger [ 
	
	^ self new
		id: anInteger;
		yourself
]

{ #category : #accessing }
DRBasicBlock >> addInstruction: anInstruction [
	
	self haltIf: [ (anInstruction isKindOf: DRNullValue) ].
	^ instructions add: anInstruction
]

{ #category : #accessing }
DRBasicBlock >> blockValue [
	
	^ blockValue
]

{ #category : #accessing }
DRBasicBlock >> blockValue: aValue [
	
	self haltIf: [ (aValue isKindOf: DRValue) not ].
	blockValue := aValue
]

{ #category : #accessing }
DRBasicBlock >> endInstruction: aDRInstruction [ 
	endInstruction := aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> firstInstruction [
	
	^ instructions first
]

{ #category : #accessing }
DRBasicBlock >> id [

	^ id
]

{ #category : #accessing }
DRBasicBlock >> id: anObject [

	id := anObject
]

{ #category : #accessing }
DRBasicBlock >> initialize [
	super initialize.	
	instructions := OrderedCollection new.
	blockValue := DRNullValue new
]

{ #category : #'as yet unclassified' }
DRBasicBlock >> inspectionInstructions: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Instructions'> 
	
	^ aBuilder newList
		display: [ :each | StObjectPrinter asTruncatedTextFrom: each prettyPrint ];
		items: self instructions;
		yourself
]

{ #category : #accessing }
DRBasicBlock >> instructions [
	^ instructions , { endInstruction }
]

{ #category : #iterating }
DRBasicBlock >> instructionsDo: aFullBlockClosure [ 

	instructions do: aFullBlockClosure.
	endInstruction ifNotNil: [ aFullBlockClosure value: endInstruction ]
]

{ #category : #building }
DRBasicBlock >> jumpTo: aBasicBlock [ 

	^ endInstruction := (DRJump new target: aBasicBlock)
]

{ #category : #accessing }
DRBasicBlock >> lastInstruction [
	
	^ endInstruction
]

{ #category : #instructions }
DRBasicBlock >> removeInstruction: aDRInstruction [
	
	instructions remove: aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> successors [
	
	^ instructions last targets
]
