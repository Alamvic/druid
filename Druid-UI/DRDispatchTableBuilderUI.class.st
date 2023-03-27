"
Main window for a Dispatch Table builder UI. It allows to filter and select primitive entries from a ""prototype class"" (acting as source of primitive methods), then after specifying the target compilation Interpreter and JIT compiler class to be generated, it will enable a ""Build Dispatch Table"" command `DRBuildTableCommand` to be executed.

To launch the UI evaluate: 

```language=Pharo
DRDispatchTableApp new start.
```

"
Class {
	#name : #DRDispatchTableBuilderUI,
	#superclass : #SpPresenter,
	#instVars : [
		'entriesListPresenter',
		'classPrototypeDropListPresenter',
		'patternEntryTextPresenter',
		'dispatchTable',
		'scanSuperclassesCheckBoxPresenter',
		'statusBar',
		'interpreterNameTextEntryPresenter',
		'jitNameTextEntryPresenter',
		'selectInterpreterSuperclassButtonPresenter',
		'selectJITSuperclassButtonPresenter',
		'interpreterSuperclassNameTextPresenter',
		'jitSuperclassTextPresenter',
		'browseButtonPresenter',
		'patternMatchMethod'
	],
	#category : #'Druid-UI'
}

{ #category : #accessing }
DRDispatchTableBuilderUI class >> buildBrowsingOperationsGroupWith: aDRDispatchTableBuilderUI [

	^ (CmCommandGroup named: 'DTBrowseOperationsMenu') asSpecGroup
		register: ((DRBrowseMethodCommand
			forSpecWithIconNamed: #class
			shortcutKey: 'B' asShortcut)
				context: aDRDispatchTableBuilderUI);
		register: ((DRBTranspileCommand
			forSpecWithIconNamed: #smallCogInitialState
			shortcutKey: 'C' asShortcut)
				context: aDRDispatchTableBuilderUI);
		beDisplayedAsGroup;
		yourself
]

{ #category : #accessing }
DRDispatchTableBuilderUI class >> buildCommandsGroupWith: presenterInstance forRoot: rootCommandGroup [

	rootCommandGroup
		register: (self buildDTContextualGroupWith: presenterInstance)
]

{ #category : #accessing }
DRDispatchTableBuilderUI class >> buildDTContextualGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'DTSelContextualMenu') asSpecGroup
		register: (self buildBrowsingOperationsGroupWith: presenterInstance);
		register: (self buildSelectionOperationsGroupWith: presenterInstance);
		register: (self buildTableGroupWith: presenterInstance);
		yourself
]

{ #category : #accessing }
DRDispatchTableBuilderUI class >> buildSelectionOperationsGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'DTSelectionOperationsMenu') asSpecGroup
		register: ((DRSelectAllCommand
			forSpecWithIconNamed: #checkedBox
			shortcutKey: 'A' asShortcut)
				context: presenterInstance);
		register: ((DRSelectNoneCommand
			forSpecWithIconNamed: #checkboxUnselected
			shortcutKey: 'N' asShortcut)
				context: presenterInstance);
		beDisplayedAsGroup;
		yourself
]

{ #category : #accessing }
DRDispatchTableBuilderUI class >> buildTableGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'DTBuildMenu') asSpecGroup
		register: ((DRBuildTableCommand
			forSpecWithIconNamed: #add
			shortcutKey: 'B' asShortcut)
				context: presenterInstance);
		beDisplayedAsGroup;
		yourself
]

{ #category : #'instance creation' }
DRDispatchTableBuilderUI class >> example [

	^ self open
]

{ #category : #'instance creation' }
DRDispatchTableBuilderUI class >> open [

	^ self new open
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> availableDispatchTablePrototypess [
	" Answer a <Collection> of all available classes which may act as prototypes for a dispatch table "

	^  InterpreterPrimitives withAllSubclasses
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> classPrototypeDropListPresenter [

	^ classPrototypeDropListPresenter
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> classPrototypeDropListPresenter: anObject [

	classPrototypeDropListPresenter := anObject
]

{ #category : #layout }
DRDispatchTableBuilderUI >> defaultLayout [

	^ self layout: (SpBoxLayout newTopToBottom

		add: (SpGridLayout new
			build: [ :builder | builder
			add: 'New Interpreter Superclass'; add: interpreterSuperclassNameTextPresenter; add: selectInterpreterSuperclassButtonPresenter; nextRow;
			add: 'New Interpreter Class Name'; add: interpreterNameTextEntryPresenter span: 2@1; nextRow;
			add: 'New JIT Superclass'; add: jitSuperclassTextPresenter; add: selectJITSuperclassButtonPresenter; nextRow;
			add: 'New JIT Class Name'; add: jitNameTextEntryPresenter span: 2@1; nextRow;
			add: 'Class Prototype'; 	add: classPrototypeDropListPresenter; add: browseButtonPresenter; nextRow ]);
			yourself;
		add: scanSuperclassesCheckBoxPresenter expand: false;
		add: (SpBoxLayout newLeftToRight 
			add: 'Methods Pattern' expand: false;
			add: patternEntryTextPresenter;
			add: patternMatchMethod expand: false;
			yourself) expand: false;
		add: 'Entries' expand: false;
		add: entriesListPresenter;
		add: statusBar expand: false;
		yourself)
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> dispatchTable [

	^ dispatchTable
		ifNil: [ dispatchTable := DRDispatchTable for: StackInterpreterPrimitives ]
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> dispatchTable: anObject [

	dispatchTable := anObject
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> entriesListPresenter [

	^ entriesListPresenter
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> entriesListPresenter: anObject [

	entriesListPresenter := anObject
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> exportMethodDisplay: method [

	^ (method hasPragmaNamed: #export:)
		ifTrue: [ (method pragmaAt: #export:) arguments first ]
		ifFalse: [ 'NA' ]
]

{ #category : #callbacks }
DRDispatchTableBuilderUI >> filterEntriesList [
	" Callback to be executed when the user types in the methods pattern text input "

	entriesListPresenter
		items: (self dispatchTable methodsFromPattern: patternEntryTextPresenter text) asOrderedCollection.
	self updateStatusBar
]

{ #category : #callbacks }
DRDispatchTableBuilderUI >> handleEntryChange: selectedMethod [
	" Do nothing for now "

]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initialize [

	super initialize.
	statusBar pushMessage: self statusBarMessage
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializeClassPrototypeSelector [

	classPrototypeDropListPresenter := self newDropList
		help: 'Select the desired prototype';
		items: self availableDispatchTablePrototypess;
		selectItem: self dispatchTable targetClass;
		whenSelectedItemChangedDo: [ : selectedClass |
			self dispatchTable targetClass: selectedClass.
			self filterEntriesList ];
		yourself.
	browseButtonPresenter := self newButton
		label: 'Browse';
		action: [ Smalltalk tools browser openOnClass: classPrototypeDropListPresenter selectedItem  ]
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializeConfigPresenters [

	interpreterNameTextEntryPresenter := self newTextInput
		placeholder: 'DRMyInterpreterClass';
		autoAccept: true;
		yourself.
	jitNameTextEntryPresenter := self newTextInput
		placeholder: 'DRMyJITClass';
		autoAccept: true;
		yourself.
	jitSuperclassTextPresenter := self newTextInput
		placeholder: 'StackToRegisterMapping';
		autoAccept: true;
		yourself.
	interpreterSuperclassNameTextPresenter := self newTextInput
		placeholder: 'StackInterpreter';
		autoAccept: true;
		yourself.
	selectInterpreterSuperclassButtonPresenter := self newButton label: 'Select';
		action: [ self openInterpreterSuperclassSelector ];
		yourself.
	selectJITSuperclassButtonPresenter := self newButton label: 'Select';
		action: [ self openJITSuperclassSelector ];
		yourself
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializeEntriesPresenter [

	entriesListPresenter := self newTable
			addColumn: (SpCheckBoxTableColumn new
				title: '';
				evaluated: [ : entry | self selectedEntries includes: entry ];
				onActivation: [ : entry | self selectedEntries add: entry. self updateStatusBar];
				onDeactivation: [ : entry | self selectedEntries remove: entry. self updateStatusBar ];
			beNotExpandable;
			width: 20;
			yourself);
		addColumn: (SpStringTableColumn new
			title: 'Class';
			evaluated: [ : method | method methodClass ];
			sortFunction: #printString ascending;
			yourself);
		addColumn: (SpStringTableColumn new
			title: 'Name';
			evaluated: [ : method | method selector ];
			sortFunction: #printString ascending;
			yourself);
		addColumn: (SpStringTableColumn new
			title: 'DNG';
			evaluated: [ : method | method hasPragmaNamed: #doNotGenerate ];
			sortFunction: #printString ascending;
			width: 60;
			yourself);
		addColumn: (SpStringTableColumn new
			title: 'Inline';
			evaluated: [ : method | self inlineMethodDisplay: method ];
			sortFunction: #printString ascending;
			width: 60;
			yourself);
		addColumn: (SpStringTableColumn new
			title: 'Export';
			evaluated: [ : method | self exportMethodDisplay: method ];
			sortFunction: #printString ascending;
			width: 60;
			yourself);
		addColumn: (SpStringTableColumn new
			title: 'RT';
			evaluated: [ : method | self returnType: method ];
			sortFunction: #printString ascending;
			width: 60;
			yourself);
		searchMatching: [ :selEntry :textToSearch | selEntry beginsWith: textToSearch ];
		whenSelectionChangedDo: [ : selection | self handleEntryChange: selection selectedItem ];
		items: OrderedCollection new;
		alternateRowsColor;
		contextMenu: [ (self rootCommandsGroup / 'DTSelContextualMenu') beRoot asMenuPresenter ];
		yourself
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializePatternEntryInput [

	patternEntryTextPresenter := self newTextInput
		placeholder: 'prim';
		whenTextChangedDo: [ self filterEntriesList ];
		yourself.
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializePatternEntryOptions [

	patternEntryTextPresenter := self newTextInput
		placeholder: 'primitiveAdd | .*Add';
		whenTextChangedDo: [ self filterEntriesList ];
		yourself.
	patternMatchMethod := self newCheckBox
		label: 'Use RegEx';
		state: false;
		whenChangedDo: [ : r |
			r
				ifTrue: [ 	self dispatchTable useRegEx ]
				ifFalse: [ self dispatchTable useBeginsWith ] ]
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializePresenters [

	super initializePresenters.
	self
		initializeScanSuperclassPresenter;
		initializeEntriesPresenter;
		initializeConfigPresenters;
		initializeStatusbar;
		initializeClassPrototypeSelector;
		initializePatternEntryOptions
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializeScanSuperclassPresenter [

	scanSuperclassesCheckBoxPresenter := self newCheckBox
		label: 'Scan superclasses';
		state: self dispatchTable scanSuperclasses;
		whenChangedDo: [ : v | self updateEntries ]
		yourself
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> initializeStatusbar [

	statusBar := SpStatusBarPresenter new
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> inlineMethodDisplay: method [

	^ (method hasPragmaNamed: #inline:)
		ifTrue: [ (method pragmaAt: #inline:) arguments first ]
		ifFalse: [ 'NA' ]
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> interpreterNameTextEntryPresenter [

	^ interpreterNameTextEntryPresenter
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> interpreterNameTextEntryPresenter: anObject [

	interpreterNameTextEntryPresenter := anObject
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> interpreterSuperclassNameTextPresenter [

	^ interpreterSuperclassNameTextPresenter
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> jitNameTextEntryPresenter [

	^ jitNameTextEntryPresenter
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> jitNameTextEntryPresenter: anObject [

	jitNameTextEntryPresenter := anObject
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> jitSuperclassTextPresenter [

	^ jitSuperclassTextPresenter
]

{ #category : #callbacks }
DRDispatchTableBuilderUI >> openInterpreterSuperclassSelector [

	| selection |

	(DRDispatchTableSuperclassSelectorUI on: AbstractInterpreter)
		title: 'Select Interpreter Superclass';
		label: 'Select a class';
		onAccept: [ :dialog | selection := dialog presenter classHierarchyPresenter selectedItem ];
		openModal.
	selection
		ifNotNil: [ interpreterSuperclassNameTextPresenter text: selection name ]
		ifNil: [ interpreterSuperclassNameTextPresenter text: String empty ]
]

{ #category : #callbacks }
DRDispatchTableBuilderUI >> openJITSuperclassSelector [

	| selection |

	(DRDispatchTableSuperclassSelectorUI on: StackToRegisterMappingCogit)
		title: 'Select JIT Superclass';
		label: 'Select a class';
		onAccept: [ :dialog | selection := dialog presenter classHierarchyPresenter selectedItem ];
		openModal.
	selection
		ifNotNil: [ jitSuperclassTextPresenter text: selection name ]
		ifNil: [ jitSuperclassTextPresenter text: String empty ]
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> patternEntryTextPresenter [

	^ patternEntryTextPresenter
]

{ #category : #accessing }
DRDispatchTableBuilderUI >> patternEntryTextPresenter: anObject [

	patternEntryTextPresenter := anObject
]

{ #category : #layout }
DRDispatchTableBuilderUI >> popMessage [

	statusBar popMessage
]

{ #category : #initialization }
DRDispatchTableBuilderUI >> returnType: method [ 

	^ (method hasPragmaNamed: #returnTypeC:) 
		ifTrue: [ (method pragmaAt: #returnTypeC:) arguments first ]
		ifFalse: [ 'NA' ]
]

{ #category : #'menu - accessing' }
DRDispatchTableBuilderUI >> selectedEntries [
	"Answer a <Collection> of entries marked for installation"

	^ self application selectedEntries

]

{ #category : #layout }
DRDispatchTableBuilderUI >> statusBarMessage [

	| listedEntriesCount selectedEntriesCount |
	
	listedEntriesCount := self entriesListPresenter 
		ifNil: [ 0 ]
		ifNotNil: [ self entriesListPresenter items size ].
	selectedEntriesCount := self application selectedEntries size.
	^ '{1} entries, {2} selected' 
		format: { 
			listedEntriesCount asString .
			selectedEntriesCount asString }
]

{ #category : #callbacks }
DRDispatchTableBuilderUI >> updateEntries [

	self dispatchTable scanSuperclasses: scanSuperclassesCheckBoxPresenter state.
	self filterEntriesList
]

{ #category : #callbacks }
DRDispatchTableBuilderUI >> updateStatusBar [

	statusBar pushMessage: self statusBarMessage.
]
