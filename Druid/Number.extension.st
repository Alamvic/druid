Extension { #name : #Number }

{ #category : #'*Druid' }
Number >> expression [
	
	^ RBLiteralValueNode value: self
]

{ #category : #'*Druid' }
Number >> isJitCompileTimeExpression [
	
	^ true
]
