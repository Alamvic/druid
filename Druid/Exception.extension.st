Extension { #name : #Exception }

{ #category : #'*Druid' }
Exception >> handlePrimitiveCompilationFor: aDRPrimitiveCCCollector [

	self halt.
	aDRPrimitiveCCCollector
		failedPrimitivesAt: self
		add: aDRPrimitiveCCCollector sourceName
]
