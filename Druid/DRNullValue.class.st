Class {
	#name : 'DRNullValue',
	#superclass : 'DRValue',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRNullValue >> = anotherValue [

	^ self species = anotherValue species
]

{ #category : 'visiting' }
DRNullValue >> acceptVisitor: aVisitor [

	^ aVisitor visitNullValue: self
]

{ #category : 'control-flow' }
DRNullValue >> addToBlock: aDRBasicBlock [ 
	
	"Nothing"
]

{ #category : 'copying' }
DRNullValue >> copyToVariableIn: aDRIRGenerator inNode: aNode [
	
	"I'm not a value, so I cannot be copied.
	I propagate myself as a null  value"
	^ self
]

{ #category : 'accessing' }
DRNullValue >> hash [

	^ self class hash
]

{ #category : 'accessing' }
DRNullValue >> id [

	^ self
]

{ #category : 'visiting' }
DRNullValue >> isNullValue [

	^ true
]

{ #category : 'control-flow' }
DRNullValue >> joinNonNull: anotherValue onIRGenerator: anIRGenerator from: originBlocks [

	^ anotherValue
]

{ #category : 'control-flow' }
DRNullValue >> joinValue: anotherValue onIRGenerator: anIRGenerator from: originBlocks [

	^ anotherValue
]

{ #category : 'accessing' }
DRNullValue >> name [
	
	^ 'NULL'
]

{ #category : 'visiting' }
DRNullValue >> rtlOperandQualifier [
	
	^ 'Null:'
]

{ #category : 'sccp' }
DRNullValue >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'asd' }
DRNullValue >> typeWithAlreadySeen: _ [
	
	^ DRUndefinedType new
]
