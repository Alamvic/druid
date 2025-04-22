Class {
	#name : 'DRNoTestMask',
	#superclass : 'DRCondition',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRNoTestMask >> acceptVisitor: aVisitor withBranch: parentBranch [
	
	^ aVisitor visitNoTestMask: self inBranch: parentBranch
]

{ #category : 'testing' }
DRNoTestMask >> isNoTestMask [
	
	^ true
]

{ #category : 'transforming' }
DRNoTestMask >> negated [
	
	^ DRTestMask new
]

{ #category : 'testing' }
DRNoTestMask >> opcode [
	
	^ 'NoTest'
]

{ #category : 'converting' }
DRNoTestMask >> reversed [
	^ self
]
