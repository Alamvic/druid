Class {
	#name : #DRBuildTableCommand,
	#superclass : #DRDispatchTableMenuCommand,
	#category : #'Druid-DispatchTable-Commands'
}

{ #category : #default }
DRBuildTableCommand class >> defaultDescription [

	^ 'Build Dispatch Table'
]

{ #category : #'accessing - defaults' }
DRBuildTableCommand class >> defaultIconName [

	^ #add
]

{ #category : #default }
DRBuildTableCommand class >> defaultName [

	^ 'Build Dispatch Table'
]

{ #category : #testing }
DRBuildTableCommand >> canBeExecuted [
	" Receiver can be executed if there are selections "

	^ self application selectedEntries notEmpty
]

{ #category : #executing }
DRBuildTableCommand >> execute [
	"Private - Open a builder UI on the selected entries"

	| selection |

	DRDispatchTableSuperclassSelectorUI 	new
		title: 'Select Interpreter Superclass';
		label: 'Select a class';
		onAccept: [ :dialog | selection := dialog presenter classHierarchyPresenter selectedItem ];
		openModal.
	selection ifNil: [ ^ self ].
	self executeForInterpreterSuperclass: selection
]

{ #category : #accessing }
DRBuildTableCommand >> executeForInterpreterSuperclass: aClass [ 

	| interpreterBuilder compilationUnit |

	self halt.
	interpreterBuilder := DRInterpreterBuilder new 
		targetSuperclass: aClass;
		basicNew;
		primitives: 	self selectedEntries asArray;
		yourself.
	compilationUnit := interpreterBuilder generateBuildModel.
	compilationUnit compileInitializePrimitiveTable.
	(DRInterpreterCompiler fromInterpreter: interpreterBuilder) generateBuildModelAndCompileIn: #MyJITCompilerClass.
	Smalltalk tools browser openOnClass: #MyJITCompilerClass asClass. 

]

{ #category : #accessing }
DRBuildTableCommand >> newRandomInterpreterClassName [
	"Answer a new class name to be used as interpreter containing a #basicPrimitiveTable"

	^ String streamContents: [ : stream |
		stream 
			<< #DRInterpreter;
			<< (UUIDGenerator next asString copyUpTo: $-) ]
]
