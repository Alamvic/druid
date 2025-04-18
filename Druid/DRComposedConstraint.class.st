Class {
	#name : 'DRComposedConstraint',
	#superclass : 'DRConditionalConstraint',
	#instVars : [
		'constraints'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'accessing' }
DRComposedConstraint class >> constraints: constraints [

	^ self new
		  constraints: constraints;
		  yourself
]

{ #category : 'accessing' }
DRComposedConstraint class >> withTypes: types [

	^ self constraints:
		  (types collect: [ :t | DRIncludeTypeConstraint withType: t ])
]

{ #category : 'accessing' }
DRComposedConstraint class >> withoutTypes: types [

	^ self constraints:
		  (types collect: [ :t | DRExcludeTypeConstraint withType: t ])
]

{ #category : 'accessing' }
DRComposedConstraint >> asDependencyList [

	constraints flatCollect: [ :constraint | constraint asDependencyList ]

]

{ #category : 'accessing' }
DRComposedConstraint >> constraints [

	^ constraints
]

{ #category : 'accessing' }
DRComposedConstraint >> constraints: anObject [

	constraints := anObject
]

{ #category : 'printing' }
DRComposedConstraint >> printOn: aStream [

	aStream
		nextPutAll: '(';
		print: constraints first;
		nextPutAll: ' ';
		nextPutAll: self opCode;
		nextPutAll: ' ';
		print: constraints second;
		nextPutAll: ')'

]
