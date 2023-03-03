Extension { #name : #CompiledMethod }

{ #category : #'*Druid' }
CompiledMethod >> numberOfArguments [

	^ (self ast pragmaNamed: #numberOfArguments:) arguments first value
]
