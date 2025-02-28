Class {
	#name : 'DRLessThanComparison',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLessThanComparison >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitLessThan: self inBranch: parentBranch
]

{ #category : 'converting' }
DRLessThanComparison >> asJitCompileTimeExpressionString [
	
	^ '<'
]

{ #category : 'testing' }
DRLessThanComparison >> isLessThanComparison [

	^ true
]

{ #category : 'transforming' }
DRLessThanComparison >> negated [
	
	^ DRGreaterOrEqualsThanComparison new
]

{ #category : 'printing' }
DRLessThanComparison >> opcode [
	
	^ 'LessThan'
]

{ #category : 'printing' }
DRLessThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x < y --> y > x"
	^ DRGreaterThanComparison new
]

{ #category : 'SCCP' }
DRLessThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand < operand2
]
