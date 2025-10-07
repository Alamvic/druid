Class {
	#name : 'DRRotateLeft',
	#superclass : 'DRArithmeticInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRRotateLeft >> acceptVisitor: aVisitor [

	^ aVisitor visitLeftRotate: self
]

{ #category : 'types' }
DRRotateLeft >> typeWithAlreadySeen: _ [

	^ DRUnsignedIntegerType bits64
]
