Class {
	#name : #DRDispatchTableMenuCommand,
	#superclass : #CmCommand,
	#category : #'Druid-DispatchTable-Commands'
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
