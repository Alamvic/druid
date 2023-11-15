Class {
	#name : #DRPointerType,
	#superclass : #DRUnsignedIntegerType,
	#category : #'Druid-IR-Types'
}

{ #category : #initialization }
DRPointerType >> initialize [

	super initialize.
	
	"64bits only for now"
	size := 8
]
