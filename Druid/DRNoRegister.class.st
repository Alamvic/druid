Class {
	#name : #DRNoRegister,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRNoRegister >> isPhysicalRegister [

	^ false
]

{ #category : #accessing }
DRNoRegister >> name [
	
	^ '_'
]
