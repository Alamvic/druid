Extension { #name : #CompiledMethod }

{ #category : #'*Druid' }
CompiledMethod >> hasPragmaNamed: pragmaName setting: argumentValue [
	" Answer <true> if receiver defines pragmaName with argumentValue as argument "

	^ (self hasPragmaNamed: pragmaName) and: [
		argumentValue matchArgumentsAgainst: (self pragmaAt: pragmaName) ]
]

{ #category : #'*Druid' }
CompiledMethod >> numberOfArguments [

	^ (self ast pragmaNamed: #numberOfArguments:) arguments first value
]
