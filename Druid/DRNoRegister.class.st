Class {
	#name : #DRNoRegister,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRNoRegister >> hasId: id [

	^ false
]

{ #category : #testing }
DRNoRegister >> isNoResult [
	
	^ true
]

{ #category : #accessing }
DRNoRegister >> name [
	
	^ '_'
]
