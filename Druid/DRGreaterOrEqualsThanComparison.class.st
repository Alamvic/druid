Class {
	#name : 'DRGreaterOrEqualsThanComparison',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRGreaterOrEqualsThanComparison >> acceptVisitor: aVisitor withBranch: parentBranch [ 

	^ aVisitor visitGreaterOrEqualsThan: self inBranch: parentBranch
]

{ #category : 'converting' }
DRGreaterOrEqualsThanComparison >> asJitCompileTimeExpressionString [

	^ '>='
]

{ #category : 'testing' }
DRGreaterOrEqualsThanComparison >> isGreaterOrEqualThanComparison [
	
	^ true
]

{ #category : 'transforming' }
DRGreaterOrEqualsThanComparison >> negated [
	
	^ DRLessThanComparison new
]

{ #category : 'printing' }
DRGreaterOrEqualsThanComparison >> opcode [

	^ 'GreaterOrEquals'
]

{ #category : 'reversing' }
DRGreaterOrEqualsThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x >= y --> y <= x"
	^ DRLessOrEqualsThanComparison new
]

{ #category : 'SCCP' }
DRGreaterOrEqualsThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand >= operand2
]
