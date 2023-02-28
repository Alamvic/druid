Class {
	#name : #DRBTranspileCommand,
	#superclass : #DRDispatchTableMenuCommand,
	#category : #'Druid-DispatchTable-Commands'
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

{ #category : #executing }
DRBTranspileCommand >> execute [
	"Private - Open a Browser on the selected method"

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
