Extension { #name : 'TestAsserter' }

{ #category : '*Druid-Tests' }
TestAsserter >> onPharoCITestingEnvironment [

	^ Smalltalk os environment at: 'PHARO_CI_TESTING_ENVIRONMENT' ifPresent: [ true ] ifAbsent: [ false ]
]
