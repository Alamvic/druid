Class {
	#name : 'DRAbstractLoad',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'types' }
DRAbstractLoad >> typeWithAlreadySeen: _ [

	^ self subclassResponsibility
]
