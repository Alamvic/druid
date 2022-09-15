Class {
	#name : #DRFloatType,
	#superclass : #DRTopType,
	#category : #'Druid-IR-Types'
}

{ #category : #converting }
DRFloatType >> asInteger [

	<druidIRBuilder: #interpretAsIntegerWith:>
]

{ #category : #testing }
DRFloatType >> isFloatType [
	
	^ true
]

{ #category : #accessing }
DRFloatType >> systemClass [
	
	^ Float
]
