Class {
	#name : #DRNoRegister,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRNoRegister >> hasId: id [

	^ false
]

{ #category : #accessing }
DRNoRegister >> name [
	
	^ '_'
]
