Class {
	#name : 'DRFloatType',
	#superclass : 'DRTopType',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'converting' }
DRFloatType >> asInteger [

	<druidIRBuilder: #interpretAsIntegerWith:>
]

{ #category : 'testing' }
DRFloatType >> isFloatType [
	
	^ true
]

{ #category : 'accessing' }
DRFloatType >> size [

	"Floats are double-precision IEEE-754 floats"
	^ 8
]

{ #category : 'accessing' }
DRFloatType >> systemClass [
	
	^ Float
]
