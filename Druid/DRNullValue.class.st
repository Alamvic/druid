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

{ #category : #copying }
DRNullValue >> copyToVariableIn: aDRIRGenerator inNode: aNode [
	
	"I'm not a value, so I cannot be copied.
	I propagate myself as a null  value"
	^ self
]

{ #category : #'control-flow' }
DRNullValue >> joinNonNull: anotherValue onIRGenerator: anIRGenerator from: originBlocks [

	^ anotherValue
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
