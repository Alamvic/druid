Extension { #name : #Integer }

{ #category : #'*Druid' }
Integer >> asDRValue [
	
	^ DRConstantValue value: self
]
