Class {
	#name : 'DRLessOrEqualsThanComparison',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLessOrEqualsThanComparison >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitLessOrEqualsThan: self inBranch: parentBranch
]

{ #category : 'converting' }
DRLessOrEqualsThanComparison >> asJitCompileTimeExpressionString [

	^ '<='
]

{ #category : 'testing' }
DRLessOrEqualsThanComparison >> isLessOrEqualThanComparison [
	
	^ true
]

{ #category : 'transforming' }
DRLessOrEqualsThanComparison >> negated [
	
	^ DRGreaterThanComparison new
]

{ #category : 'printing' }
DRLessOrEqualsThanComparison >> opcode [
	
	^ 'LessOrEqualsThan'
]

{ #category : 'printing' }
DRLessOrEqualsThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x <= y --> y >= x"
	^ DRGreaterOrEqualsThanComparison new
]

{ #category : 'SCCP' }
DRLessOrEqualsThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand sccpLessThanOrEquals: operand2
]
