Extension { #name : #Context }

{ #category : #'*Druid' }
Context >> deoptimizeSender [
	
	| info |

	info := sender method deoptimizationInformation.
	info ifNil: [ ^ sender ].
	
	sender := info deoptimizeContext: sender.
		
	^ sender 
]
