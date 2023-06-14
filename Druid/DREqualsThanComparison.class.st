Class {
	#name : #DREqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DREqualsThanComparison >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitEqualsThan: self inBranch: parentBranch
]

{ #category : #converting }
DREqualsThanComparison >> asJitCompileTimeExpressionString [

	^ '=' 
]

{ #category : #testing }
DREqualsThanComparison >> isCommutative [
	
	^ true
]

{ #category : #testing }
DREqualsThanComparison >> isEqualsThanComparison [
	
	^ true
]

{ #category : #transforming }
DREqualsThanComparison >> negated [
	
	^ DRNotEqualsThanComparison new
]

{ #category : #printing }
DREqualsThanComparison >> opcode [
	
	^ 'EqualsThan'
]

{ #category : #printing }
DREqualsThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x = y --> y = x"
	^ DREqualsThanComparison new
]

{ #category : #SCCP }
DREqualsThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand sccpEquals: operand2
]
