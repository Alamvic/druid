Class {
	#name : #DRNullValue,
	#superclass : #DRValue,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNullValue >> acceptVisitor: aVisitor [

	^ aVisitor visitNullValue: self
]

{ #category : #'control-flow' }
DRNullValue >> addToBlock: aDRBasicBlock [ 
	
	"Nothing"
]

{ #category : #'control-flow' }
DRNullValue >> joinValue: aValue onIRGenerator: anObject [
	
	^ aValue
]

{ #category : #'control-flow' }
DRNullValue >> joinValue: anotherValue onIRGenerator: anIRGenerator from: originBlocks [

	^ anotherValue
]

{ #category : #accessing }
DRNullValue >> name [
	
	^ 'NULL'
]
