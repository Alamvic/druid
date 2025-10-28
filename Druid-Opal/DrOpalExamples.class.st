Class {
	#name : 'DrOpalExamples',
	#superclass : 'Object',
	#instVars : [
		'dummyVariable'
	],
	#classVars : [
		'classVariable'
	],
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicInlineMethod [

	^ self lazyDouble: 42
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicInlineMethodAccesInstVar [
	<var: #aStack type: #Stack >
	
	| aStack |
	aStack := Stack new.
	aStack push: 'element'.
	
	^ aStack firstLink 
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicInlineMethodSelfAccesInstVar [
	
	^ dummyVariable
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicInlineMethodSelfSettingInstVar [

	dummyVariable := 5.
	^ dummyVariable
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicInlineMethodSelfSettingInstVarOnReturn [

	^ dummyVariable := 5
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicInlineMethodSelfSettingInstVarWithUsers [

	| tempVariable |
	^ tempVariable := dummyVariable := 5
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicInlineMethodSettingInstVar [
	<var: #assoc type: #Association  >
	
	| assoc |
	assoc := Association new.
	assoc value: 'aNewValue'.
	^ assoc value = 'aNewValue'
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicMethodWithBlock2: aBlock [

	| tmp2 |
	tmp2  := aBlock value.
	^ tmp2
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicMethodWithBlock: aBlock [

	| tmp1 |
	tmp1  := aBlock value.
	^ tmp1
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> basicMethodWithBlockArg: aBlock [

	| tmp1 |
	tmp1  := aBlock value: 5.
	^ tmp1
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> blockWithValue: arg [
	^ [ arg ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> empty [
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> lazyDouble: n [

	<druidDeferredInline>
	^ n * 2
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockConstant [
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ 2 ].
	^ temp
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockLocalAndNonLocalReturn [
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ | x | 
		x ifNil: [ ^ 2 ].
		4 ].
	^ 5
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2ReadAndWrite [

	| temp1 temp2|
	temp1 := 1.
	temp2 := 2.
	self basicMethodWithBlock: [ 
		| x |
		x := temp1.
		temp2 := 2.
		self basicMethodWithBlock: [ 
			| y |
			y := temp2.
			temp1 := x. 
			]
		 ].
	^ temp2.
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2ReadAndWriteWithArg: arg [

	| temp1 temp2|
	temp1 := 1.
	temp2 := 2.
	self basicMethodWithBlockArg: [ :arg1 |
		| x |
		x := arg.
		temp2 := 2.
		self basicMethodWithBlock: [ 
			| y |
			y := arg1.
			temp1 := x. 
			temp2 := arg
			]
		 ].
	^ temp2.
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2ReadingOuterArg2: arg1 [

	^ self basicMethodWithBlock: [
		self basicMethodWithBlock: [
			| y |
			y := arg1 ] ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2ReadingOuterArg: arg1 [

	^ self basicMethodWithBlock: [
		| x |
		x := arg1.
		self basicMethodWithBlock: [
			| y |
			y := arg1 ] ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2ReadingOuterTemp [

	| temp |
	temp := 5.
	^ self basicMethodWithBlock: [
		| x |
		x := temp.
		self basicMethodWithBlock: [
			| y |
			y := temp ] ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2ReadingOuterTemp2 [

	| temp |
	temp := 5.
	^ self basicMethodWithBlock: [
		  self basicMethodWithBlock: [
			  | y |
			  y := temp ] ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2Simple [

	^ self basicMethodWithBlock: [ self basicMethodWithBlock: [ 0 ] ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2WithArg [

	^ self basicMethodWithBlockArg: [ :arg1 |
		  self basicMethodWithBlockArg: [ :arg2 | 
			arg2 ]
		 ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2WithArg2 [

	^ self basicMethodWithBlockArg: [ :arg1 |
		  self basicMethodWithBlock: [ 
			arg1]
		 ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2WritingOuterTemp [

	| temp |
	temp := 0.
	self basicMethodWithBlock: [
		temp := 1.
		self basicMethodWithBlock: [ temp := 6 ] ].
	^ temp
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNested2WritingOuterTemp2 [

	| temp |
	temp := 0.
	self basicMethodWithBlock: [
		self basicMethodWithBlock: [ temp := 6 ] ].
	^ temp
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockNonLocalReturn [
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ ^ 2 ].
	^ 5
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodBlockTwoOuterScopes [

	| temp block |
	temp := 1.
	block := self blockWithValue: temp.
	^ block value
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodCollectionSize: aCollection [
	<var: #aCollection type: #OrderedCollection>
	<var: #aCollection type: #Array2D>
	
	^ aCollection size
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodEmptyBlock [ 
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ ].
	^ temp
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodInlineThenSendClass: aCollection [

	<var: #aCollection type: #Array>
	^ aCollection className
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodReadingClassVariable [

	^ classVariable
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithArg: aCollection [
	<var: #aCollection type: #OrderedCollection>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithDynamicArray [
	
	^ { 1 . 2 . 3+2 }
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithEarlyReturn: n [

	n = 0 ifTrue: [ ^ 42 ].
	^ 57
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithMultipleTypeAnnotations: aCollection [

	<var: #aCollection type: #OrderedCollection>
	<var: #aCollection type: #Dictionary>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithNoTypeAnnotation: aCollection [
	
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithThreeNonLocalReturn [

	true
		ifTrue: [ ^ 2 ]
		ifFalse: [ ^ 3 ].
	^ 1
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithToDo [

	| n |
	n := 0.
	1 to: 10 do: [ :i | n := n + i ].
	^ n
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithTwoToDos [

	| n |
	n := 0.
	1 to: 10 do: [ :i | 1 to: 10 do: [ :j | n := n + i + j ] ].
	^ n
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithUnknownTypeAnnotation: aCollection [

	<var: #aCollection type: #OrderedCollection>
	<var: #aCollection type: #Unknown>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWithUnknownTypeAnnotationBeforeConcreteTypeAnnotation: aCollection [

	<var: #aCollection type: #Unknown>
	<var: #aCollection type: #Dictionary>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> methodWritingClassVariable [
	
	classVariable := 1.
	
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethod [

	| x y| 
	x := 1.
	y := 2.
	
	^ x + y
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodArgWithBlockWithArg: arg [
	
	self basicMethodWithBlockArg: [ :argBlock |
		| x | 
		x:=arg.
		x].
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodReturnArg: arg [
	
	^ arg
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWith1Arg: argument [

	| x | 
	x := 1.
	
	^ x + argument
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWith1Arg: argument1 arg2: argument2 [

	| x| 
	x := 1.
	
	^ x + argument1 + argument2
	
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlock [

	| tmp |
	tmp := self basicMethodWithBlock: [ | x | 
		x:=5.
		x].
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockActivated [

	| tmp |
	tmp := [ | x | 
		x:=5.
		x] value.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockActivatedArg [

	| tmp |
	tmp := [ :arg |
	       | x |
	       x := 5.
	       x ] value: 2.

	^ tmp
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockCull [

	| tmp |
	tmp := [ 5 ] cull: 0.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockCull1Arg [

	| tmp |
	tmp := [ :arg1 |
		arg1] cull: 0.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockCull2Arg [

	| tmp |
	tmp := [ :arg1 |
		arg1] cull: 0 cull:1.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockExperimental: arg [

	self basicMethodWithBlock: [ :arg2 |
		|x|
		x := arg.
		self basicMethodWithBlock: [ | y |
			y := arg2.
			y := arg ]
	 ].
	
	^ arg
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockReadingArgVariable: arg [

	self basicMethodWithBlock: [ |x|.
		x:=arg.
		x].
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockReadingOuterVariable [

	| temp |
	temp:=0.
	self basicMethodWithBlock: [ |x|.
		x:=temp.
		x].
	
	^ temp
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockWithArg [
	
	^ self basicMethodWithBlockArg: [ :arg |
		| x | 
		x:=arg + 1.
		x].
	
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockWritingOuterVariable [

	| temp |
	temp:=0.
	self basicMethodWithBlock: [
		temp := 5.
		].
	
	^ temp
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithBlockWritingOuterVariable: arg [

	| temp read|
	temp:=0.
	read:=5.
	self basicMethodWithBlock: [ |x|
		x := read.
		temp := 5.
		temp.
		self basicMethodWithBlock: [
		read:=1.
		x:=0.
		temp := 5.
		temp.
		].
		].
	
	^ temp
	
]

{ #category : 'as yet unclassified' }
DrOpalExamples >> simpleMethodWithTempAssignment [

	| x y tmp| 
	x := 1.
	tmp := y := 2.
	
	^ x + y
	
	
]
