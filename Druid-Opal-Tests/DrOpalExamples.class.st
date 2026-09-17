Class {
	#name : 'DROpalExamples',
	#superclass : 'Object',
	#instVars : [
		'dummyVariable',
		'fillMask',
		'ncol'
	],
	#classVars : [
		'classVariable'
	],
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'as yet unclassified' }
DROpalExamples >> basicInlineMethod [

	^ self doubleOf: 42
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicInlineMethodAccesInstVar [
	<var: #aStack type: #Stack >
	
	| aStack |
	aStack := Stack new.
	aStack push: 'element'.
	
	^ aStack firstLink 
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicInlineMethodSelfAccesInstVar [
	
	^ dummyVariable
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicInlineMethodSelfSettingInstVar [

	dummyVariable := 5.
	^ dummyVariable
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicInlineMethodSelfSettingInstVarOnReturn [

	^ dummyVariable := 5
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicInlineMethodSelfSettingInstVarWithUsers [

	| tempVariable |
	^ tempVariable := dummyVariable := 5
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicInlineMethodSettingInstVar [
	<var: #assoc type: #Association  >
	
	| assoc |
	assoc := Association new.
	assoc value: 'aNewValue'.
	^ assoc value = 'aNewValue'
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicMethodWithBlock2: aBlock [

	| tmp2 |
	tmp2  := aBlock value.
	^ tmp2
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicMethodWithBlock: aBlock [

	| tmp1 |
	tmp1  := aBlock value.
	^ tmp1
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> basicMethodWithBlockArg: aBlock [

	| tmp1 |
	tmp1  := aBlock value: 5.
	^ tmp1
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> blockWithValue: arg [
	^ [ arg ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> complexInlineMethod [

	| x |
	x := 0.
	x = 0 ifTrue: [ ].
	self whileWithMessageWithBlock: 0
]

{ #category : 'displaying' }
DROpalExamples >> displayString [ 

	^ super displayString , ' cool'
]

{ #category : 'as yet unclassified' }
DROpalExamples >> doubleOf: n [

	^ n * 2
]

{ #category : 'as yet unclassified' }
DROpalExamples >> doubleWhileWithBlock: aBlock [
    | x y |
    [
    	[ x > y ifFalse: [aBlock value: x].
    x > y] whileTrue: [x := x + 1].
    x < y] whileTrue: [y := y + 1]
]

{ #category : 'accessing' }
DROpalExamples >> dummyVariable [

	^ dummyVariable
]

{ #category : 'accessing' }
DROpalExamples >> dummyVariable: anObject [

	dummyVariable := anObject
]

{ #category : 'as yet unclassified' }
DROpalExamples >> empty [
]

{ #category : 'as yet unclassified' }
DROpalExamples >> emptyIf [
	| irow |
	irow = nil
		ifTrue: [ ]
		ifFalse: [ ]

]

{ #category : 'accessing' }
DROpalExamples >> fillMask [

	^ fillMask
]

{ #category : 'accessing' }
DROpalExamples >> fillMask: anObject [

	fillMask := anObject
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockConstant [
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ 2 ].
	^ temp
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockLocalAndNonLocalReturn [
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ | x | 
		x ifNil: [ ^ 2 ].
		4 ].
	^ 5
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2ReadAndWrite [

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
DROpalExamples >> methodBlockNested2ReadAndWriteWithArg: arg [

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
DROpalExamples >> methodBlockNested2ReadingOuterArg2: arg1 [

	^ self basicMethodWithBlock: [
		self basicMethodWithBlock: [
			| y |
			y := arg1 ] ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2ReadingOuterArg: arg1 [

	^ self basicMethodWithBlock: [
		| x |
		x := arg1.
		self basicMethodWithBlock: [
			| y |
			y := arg1 ] ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2ReadingOuterTemp [

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
DROpalExamples >> methodBlockNested2ReadingOuterTemp2 [

	| temp |
	temp := 5.
	^ self basicMethodWithBlock: [
		  self basicMethodWithBlock: [
			  | y |
			  y := temp ] ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2Simple [

	^ self basicMethodWithBlock: [ self basicMethodWithBlock: [ 0 ] ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2WithArg [

	^ self basicMethodWithBlockArg: [ :arg1 |
		  self basicMethodWithBlockArg: [ :arg2 | 
			arg2 ]
		 ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2WithArg2 [

	^ self basicMethodWithBlockArg: [ :arg1 |
		  self basicMethodWithBlock: [ 
			arg1]
		 ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2WritingOuterTemp [

	| temp |
	temp := 0.
	self basicMethodWithBlock: [
		temp := 1.
		self basicMethodWithBlock: [ temp := 6 ] ].
	^ temp
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNested2WritingOuterTemp2 [

	| temp |
	temp := 0.
	self basicMethodWithBlock: [
		self basicMethodWithBlock: [ temp := 6 ] ].
	^ temp
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockNonLocalReturn [
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ ^ 2 ].
	^ 5
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodBlockTwoOuterScopes [

	| temp block |
	temp := 1.
	block := self blockWithValue: temp.
	^ block value
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodCollectionSize: aCollection [
	<var: #aCollection type: #OrderedCollection>
	<var: #aCollection type: #Bag>
	
	^ aCollection size
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodEmptyBlock [ 
	"a constant block closure"
	
	| temp |
	temp:= self basicMethodWithBlock: [ ].
	^ temp
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodInlineThenSendClass: aCollection [

	<var: #aCollection type: #Array>
	^ aCollection className
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodInlineUnresolvedBlockNonLocalReturn [

	^ self methodBlockNonLocalReturn 
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodReadingClassVariable [

	^ classVariable
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithArg: aCollection [
	<var: #aCollection type: #OrderedCollection>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithAssignmentExpression [

	| irow |
	dummyVariable := #(1 2 3).
	irow := 0.
	dummyVariable at: (irow := irow + 1)
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithDynamicArray [
	
	^ { 1 . 2 . 3+2 }
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithEarlyReturn: n [

	n = 0 ifTrue: [ ^ 42 ].
	^ 57
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithMultipleTypeAnnotations: aCollection [

	<var: #aCollection type: #OrderedCollection>
	<var: #aCollection type: #Dictionary>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithNoTypeAnnotation: aCollection [
	
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithThreeNonLocalReturn [

	true
		ifTrue: [ ^ 2 ]
		ifFalse: [ ^ 3 ].
	^ 1
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithToDo [

	| n |
	n := 0.
	1 to: 10 do: [ :i | n := n + i ].
	^ n
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithTwoToDos [

	| n |
	n := 0.
	1 to: 10 do: [ :i | 1 to: 10 do: [ :j | n := n + i + j ] ].
	^ n
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithUnknownTypeAnnotation: aCollection [

	<var: #aCollection type: #OrderedCollection>
	<var: #aCollection type: #Unknown>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithUnknownTypeAnnotationBeforeConcreteTypeAnnotation: aCollection [

	<var: #aCollection type: #Unknown>
	<var: #aCollection type: #Dictionary>
	^ aCollection select: [ :element | element even ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithWhileAndIf [

    | x |
	[ x = 0]
        whileFalse: [
				x = nil
                ifTrue: [x:=1]
                ifFalse: [x:=0]
         ]
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWithoutInnerDependencies [

	| x |
	x := true.
	self methodWithWhileAndIf
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> methodWritingClassVariable [
	
	classVariable := 1.
	
	
	
]

{ #category : 'accessing' }
DROpalExamples >> ncol [

	^ ncol
]

{ #category : 'accessing' }
DROpalExamples >> ncol: anObject [

	ncol := anObject
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnArgument: arg [

	^ arg
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnIfNotNilValue: value [

	value ifNotNil: [ :v | ^ v ].
	^ 42
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnIfTrueIfFalseValue: value [

	^ value ifTrue: [ 5 ] ifFalse: [ 42 ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnIfTrueNonLocalReturn: value [

	value ifTrue: [ ^ 5 ].
	^ 42
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnIfTrueValue: value [

	^ value ifTrue: [ 5 ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnInitializedTemp [

	| temp |
	temp := 17.
	^ temp
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnInstanceVariable [

	^ dummyVariable
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnInstanceVariableAssignmentExpression [

	^ dummyVariable := 17
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnLiteralInteger [

	^ 1
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnNestedInstanceVariableAssignmentExpression [

	^ fillMask := dummyVariable := 17
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnNestedTempAssignmentExpression [

	| temp temp2 |
	
	^ temp := temp2 := 17
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnTempAssignmentExpression [

	| temp |
	
	^ temp := 17
]

{ #category : 'as yet unclassified' }
DROpalExamples >> returnUninitializedTemp [

	| temp |

	^ temp
]

{ #category : 'as yet unclassified' }
DROpalExamples >> sendMessage [

	self incrementCounter
]

{ #category : 'as yet unclassified' }
DROpalExamples >> setInstanceVariable [

	dummyVariable := 179
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethod [

	| x y| 
	x := 1.
	y := 2.
	
	^ x + y
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodArgWithBlockWithArg: arg [
	
	self basicMethodWithBlockArg: [ :argBlock |
		| x | 
		x:=arg.
		x].
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWith1Arg: argument [

	| x | 
	x := 1.
	
	^ x + argument
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWith1Arg: argument1 arg2: argument2 [

	| x| 
	x := 1.
	
	^ x + argument1 + argument2
	
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlock [

	| tmp |
	tmp := self basicMethodWithBlock: [ | x | 
		x:=5.
		x].
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockActivated [

	| tmp |
	tmp := [ | x | 
		x:=5.
		x] value.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockActivatedArg [

	| tmp |
	tmp := [ :arg |
	       | x |
	       x := 5.
	       x ] value: 2.

	^ tmp
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockCull [

	| tmp |
	tmp := [ 5 ] cull: 0.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockCull1Arg [

	| tmp |
	tmp := [ :arg1 |
		arg1] cull: 0.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockCull2Arg [

	| tmp |
	tmp := [ :arg1 |
		arg1] cull: 0 cull:1.
	
	^ tmp
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockExperimental: arg [

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
DROpalExamples >> simpleMethodWithBlockReadingArgVariable: arg [

	self basicMethodWithBlock: [ |x|.
		x:=arg.
		x].
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockReadingOuterVariable [

	| temp |
	temp:=0.
	self basicMethodWithBlock: [ |x|.
		x:=temp.
		x].
	
	^ temp
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockWhileTrue [

	| a |
	a := true.
	^ [a] whileTrue: [ | tmp | a:=false. tmp:=2. tmp]
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockWhileTrueWithTemp [

	| index collection|
	collection := OrderedCollection new.
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp var|
		var :=0.
		temp := index := index + 1.
		collection add: [ temp ] ].
	^collection "#(1 2 3 4 5)"
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockWithArg [
	
	^ self basicMethodWithBlockArg: [ :arg |
		| x | 
		x:=arg + 1.
		x].
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockWritingOuterVariable [

	| temp |
	temp:=0.
	self basicMethodWithBlock: [
		temp := 5.
		].
	
	^ temp
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> simpleMethodWithBlockWritingOuterVariable: arg [

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
DROpalExamples >> simpleMethodWithTempAssignment [

	| x y tmp| 
	x := 1.
	tmp := y := 2.
	
	^ x + y
	
	
]

{ #category : 'as yet unclassified' }
DROpalExamples >> storeIfTrue: value [

	| t |
	t := 42.
	value ifTrue: [ t := 5 ].
	^ t
]

{ #category : 'as yet unclassified' }
DROpalExamples >> storeIfTrueIfFalseValue: value [

	| t |
	t := value ifTrue: [ 5 ] ifFalse: [ 42 ].
	^ t
]

{ #category : 'as yet unclassified' }
DROpalExamples >> storeIfTrueValue: value [

	| t |
	t := value ifTrue: [ 5 ].
	^ t
]

{ #category : 'as yet unclassified' }
DROpalExamples >> storeInstVarIfTrueIfFalseValue: value [

	dummyVariable := value ifTrue: [ 5 ] ifFalse: [ 42 ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> storeInstVarIfTrueValue: value [

	dummyVariable := value ifTrue: [ 5 ]
]

{ #category : 'as yet unclassified' }
DROpalExamples >> storeToDo [

	| t |
	t := 0.
	1 to: 5 do: [ :n | t := n ].
	^ t
]

{ #category : 'as yet unclassified' }
DROpalExamples >> storeWhileTrue [

	| t |
	t := 0.
	[ t < 5 ] whileTrue: [ t := t + 1 ].
	^ t
]

{ #category : 'as yet unclassified' }
DROpalExamples >> whileWithMessageWithBlock: arg [

   [arg = nil]
    whileTrue: [	
		self basicMethodWithBlock: [ dummyVariable := 1 ]
	]
]
