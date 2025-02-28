Class {
	#name : 'DRGreaterThanComparison',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRGreaterThanComparison >> acceptVisitor: aVisitor withBranch: aDRBranchIfCondition [ 
	
	^ aVisitor visitGreaterThan: self inBranch: aDRBranchIfCondition
]

{ #category : 'converting' }
DRGreaterThanComparison >> asJitCompileTimeExpressionString [

	^ '>'
]

{ #category : 'testing' }
DRGreaterThanComparison >> isGreaterThanComparison [

	^ true
]

{ #category : 'transforming' }
DRGreaterThanComparison >> negated [
	
	^ DRLessOrEqualsThanComparison new
]

{ #category : 'printing' }
DRGreaterThanComparison >> opcode [
	
	^ 'GreaterThan'
]

{ #category : 'reversing' }
DRGreaterThanComparison >> reversed [

	"Returns the same condition but with reversed operands: x > y --> y < x"
	^ DRLessThanComparison new
]

{ #category : 'SCCP' }
DRGreaterThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand > operand2
]
