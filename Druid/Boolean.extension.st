Extension { #name : #Boolean }

{ #category : #'*Druid' }
Boolean >> asDRValue [
	
	^ DRConstantValue value: self
]
