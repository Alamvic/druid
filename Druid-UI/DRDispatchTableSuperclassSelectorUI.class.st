Class {
	#name : #DRDispatchTableSuperclassSelectorUI,
	#superclass : #SpSelectDialog,
	#instVars : [
		'classHierarchyPresenter',
		'rootClass'
	],
	#category : #'Druid-UI'
}

{ #category : #layout }
DRDispatchTableSuperclassSelectorUI class >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		borderWidth: 5;
		spacing: 5;
		add: #label expand: false;
		add: #classHierarchyPresenter;
		yourself
]

{ #category : #examples }
DRDispatchTableSuperclassSelectorUI class >> example [

	self new
		title: 'Select Interpreter Superclass';
		label: 'Select a class';
		onAccept: [ :dialog | self inform: dialog presenter classHierarchyPresenter selectedItem asString ];
		openDialog
]

{ #category : #accessing }
DRDispatchTableSuperclassSelectorUI >> classHierarchyPresenter [

	^ classHierarchyPresenter
]

{ #category : #accessing }
DRDispatchTableSuperclassSelectorUI >> classHierarchyPresenter: anObject [

	classHierarchyPresenter := anObject
]

{ #category : #initialization }
DRDispatchTableSuperclassSelectorUI >> initializePresenters [

	super initializePresenters.
	classHierarchyPresenter := self newTreeTable
		addColumn: (SpCompositeTableColumn new
			title: 'Classes';
			addColumn: ((SpImageTableColumn
				evaluated: [ :aClass | self iconNamed: aClass systemIconName ])
				width: 20;
				yourself);
			addColumn: (SpStringTableColumn evaluated: #name);
			yourself);
		roots: { rootClass };
		children: [ :aClass | aClass subclasses ];
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | selection selectedItem browse ];
		expandAll
]

{ #category : #'accessing - model' }
DRDispatchTableSuperclassSelectorUI >> setModelBeforeInitialization: aRootClass [

	rootClass := aRootClass
]
