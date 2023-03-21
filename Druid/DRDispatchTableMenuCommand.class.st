"
Abstract class providing access to the `DRDispatchTableBuilderUI` presenter's state.

Subclasses implement operations accessed through right-click over the list of entries (primitives).
"
Class {
	#name : #DRDispatchTableMenuCommand,
	#superclass : #CmCommand,
	#category : #'Druid-Commands'
}

{ #category : #accessing }
DRDispatchTableMenuCommand >> application [

	^ self context application
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> entriesTable [

	^ 	self context entriesListPresenter.
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> entriesTableItems [

	^ self entriesTable items
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> interpreterClassName [
	"Answer a <String> representing the interpreter class to be generated"

	^ self context interpreterNameTextEntryPresenter text
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> interpreterSuperclassName [
	"Answer a <String> representing the interpreter superclass to be generated"

	^ self context interpreterSuperclassNameTextPresenter text
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> jitClassName [
	"Answer a <String> representing the JIT class to be generated"

	^ self context jitNameTextEntryPresenter text
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> jitSuperclassName [
	"Answer a <String> representing the JIT superclass to be generated"

	^ self context jitSuperclassTextPresenter text
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> selectedEntries [

	^ self application selectedEntries
]

{ #category : #executing }
DRDispatchTableMenuCommand >> selectedEntry [

	^ self entriesTable selectedItem.
]

{ #category : #accessing }
DRDispatchTableMenuCommand >> stBrowser [

	^ Smalltalk tools browser
]

{ #category : #executing }
DRDispatchTableMenuCommand >> updateEntries [

	self entriesTable refresh.
	self context updateStatusBar
]
