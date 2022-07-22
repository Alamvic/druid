Class {
	#name : #DRNotEqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNotEqualsThanComparison >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitNotEqualsThan: self inBranch: parentBranch
]

{ #category : #testing }
DRNotEqualsThanComparison >> isNotEqualsThanComparison [
	
	^ true
]

{ #category : #transforming }
DRNotEqualsThanComparison >> negated [
	
	^ DREqualsThanComparison new
]

{ #category : #printing }
DRNotEqualsThanComparison >> opcode [
	
	^ 'NotEqualsThan'
]

{ #category : #printing }
DRNotEqualsThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x ~= y --> y ~= x"
	^ DRNotEqualsThanComparison new
]
