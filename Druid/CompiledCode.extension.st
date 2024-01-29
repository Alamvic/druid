Extension { #name : #CompiledCode }

{ #category : #'*Druid' }
CompiledCode >> deoptimizationInformation [
	
	^ self literals detect: [ :e | e isKindOf: DRDeoptimizationInformation ] ifNone: [ nil ]

]
