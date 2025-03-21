Class {
	#name : 'DRUnsignedIntegerType',
	#superclass : 'DRIntegerType',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'interpreting' }
DRUnsignedIntegerType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]

{ #category : 'testing' }
DRUnsignedIntegerType >> isSigned [
	
	^ false
]

{ #category : 'testing' }
DRUnsignedIntegerType >> isUnsigned [
	
	^ true
]

{ #category : 'accessing' }
DRUnsignedIntegerType >> systemClass [
	
	^ Integer
]
