Class {
	#name : #DRTestMask,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRTestMask >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitTestMask: self inBranch: parentBranch
]

{ #category : #converting }
DRTestMask >> asJitCompileTimeExpressionString [

	^ ' anyMask: '
]

{ #category : #testing }
DRTestMask >> isTestMask [
	
	^ true
]

{ #category : #transforming }
DRTestMask >> negated [
	
	^ DRNoTestMask new
]

{ #category : #printing }
DRTestMask >> opcode [
	
	^ 'Test'
]

{ #category : #converting }
DRTestMask >> reversed [

	"Returns the same condition but with reversed operands: Test x y --> Test y x"
	^ DRTestMask new
]

{ #category : #SCCP }
DRTestMask >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand anyMask: operand2
]
