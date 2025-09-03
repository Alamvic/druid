Class {
	#name : 'DRSignedIntegerType',
	#superclass : 'DRIntegerType',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'interpreting' }
DRSignedIntegerType >> bitShift: shiftValue [

	^ shiftValue > 0
		ifTrue: [ self << shiftValue ]
		ifFalse: [ self >> shiftValue negated ]
]

{ #category : 'accessing' }
DRSignedIntegerType >> isSigned [

	^ true
]

{ #category : 'testing' }
DRSignedIntegerType >> isUnsigned [
	
	^ false
]

{ #category : 'accessing' }
DRSignedIntegerType >> systemClass [
	
	^ Integer
]
