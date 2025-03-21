Extension { #name : 'Exception' }

{ #category : '*Druid' }
Exception >> handlePrimitiveCompilationFor: aDRPrimitiveCCCollector [

	aDRPrimitiveCCCollector
		failedPrimitivesAt: self
		add: aDRPrimitiveCCCollector sourceName
]
