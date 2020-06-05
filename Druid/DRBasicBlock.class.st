Class {
	#name : #DRBasicBlock,
	#superclass : #Object,
	#instVars : [
		'instructions'
	],
	#category : #'Druid-Instructions'
}

{ #category : #accessing }
DRBasicBlock >> appendInstruction: anInstruction [

	instructions add: anInstruction
]

{ #category : #'as yet unclassified' }
DRBasicBlock >> gtInspectorInstructionsIn: composite [
	<gtInspectorPresentationOrder: 0>
	^ composite fastTable 
		title: 'Instructions';
		display: [ instructions ];
		column: 'Instruction' evaluated: [ :e | e yourself ]
]

{ #category : #accessing }
DRBasicBlock >> id [
	
	^ self identityHash asString
]

{ #category : #accessing }
DRBasicBlock >> initialize [

	super initialize.
	instructions := OrderedCollection new
]

{ #category : #accessing }
DRBasicBlock >> instructions [
	^ instructions
]

{ #category : #accessing }
DRBasicBlock >> lastInstruction [
	
	^ instructions last
]

{ #category : #printing }
DRBasicBlock >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: '(';
		nextPutAll: self id asString;
		nextPutAll: ')'
]
