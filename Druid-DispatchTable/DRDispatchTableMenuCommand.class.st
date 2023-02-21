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
DRDispatchTableMenuCommand >> stBrowser [

	^ Smalltalk tools browser
]
