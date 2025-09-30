Class {
	#name : 'DRSignedCoercion',
	#superclass : 'DRUnaryInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRSignedCoercion >> acceptVisitor: aVisitor [
	
	^ aVisitor visitSignedCoercion: self
]

{ #category : 'printing' }
DRSignedCoercion >> opcode [
	
	^ '(signed)'
]

{ #category : 'printing' }
DRSignedCoercion >> sccpOperateOnLattice: operand [

	self assert: operand isNumber.
	^ operand
]

{ #category : 'printing' }
DRSignedCoercion >> typeWithAlreadySeen: aCollection [

	^ DRSignedIntegerType size:
		  (self operand1 typeWithAlreadySeen: aCollection) size
]
