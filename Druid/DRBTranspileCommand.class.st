"
When executed this command open a Workspace with the contents of the selected entry translated to C, using the `CCodeGenerator`.

"
Class {
	#name : #DRBTranspileCommand,
	#superclass : #DRDispatchTableMenuCommand,
	#category : #'Druid-Commands'
}

{ #category : #'accessing - defaults' }
DRBTranspileCommand class >> defaultDescription [

	^ 'Translate to C'
]

{ #category : #'accessing - defaults' }
DRBTranspileCommand class >> defaultIconName [

	^ #smallCogInitialState
]

{ #category : #'accessing - defaults' }
DRBTranspileCommand class >> defaultName [

	^ 'Translate'
]

{ #category : #testing }
DRBTranspileCommand >> canBeExecuted [ 

	^ self entriesTableItems notEmpty
]

{ #category : #executing }
DRBTranspileCommand >> execute [
	"Private - Open a workspace with C code translated from the selected method"

	Smalltalk tools workspace openContents: self translatedEntry surroundedBySingleQuotes
]

{ #category : #executing }
DRBTranspileCommand >> translatedEntry [
	" Answer a <String> with a translated representation of the receiver's selection "

	^ String streamContents: [ : stream | 
			| cg tm |		
			cg := CCodeGenerator new.
			tm := cg
				compileToTMethodSelector: self selectedEntry selector
				in: self selectedEntry methodClass.
			(tm asCASTIn: cg) prettyPrintOn: stream 	].

]
