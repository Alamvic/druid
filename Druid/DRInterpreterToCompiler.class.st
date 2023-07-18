"
Implements a basic interpreter to compiler (it doesn't provide error handling capabilities) using interpreter primitives provided in a class. The interpreter to compiler generates a new class with Druid JITted versions of the primitive methods.

Its basic usage is:

```language=Pharo
| interpreterToCompiler |

"" Instantiate an interpreter to compiler providing an AbstractInterpreter subclass with primitives ""
interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives.

"" Generate the Druid intermediate representation and compile methods in a new class ""
interpreterToCompiler generateIRAndCompileIn: #MyCoolJITCompilerClass

"" Browse the generated class ""
Smalltalk tools browser openOn: #MyCoolJITCompilerClass.
```



"
Class {
	#name : #DRInterpreterToCompiler,
	#superclass : #DRAbstractCompilerBuilder,
	#instVars : [
		'memory',
		'primitiveSelector',
		'failFirst',
		'bytecodeCompiler',
		'bytecodeSelector',
		'errors'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'instance creation' }
DRInterpreterToCompiler class >> fromInterpreterClass: anInterpreterClass [

	^ self new
		  interpreterClass: anInterpreterClass;
		  yourself
]

{ #category : #'instance creation' }
DRInterpreterToCompiler class >> generateDruidJITModel [

	<script>
	| primitives bytecodes |
	primitives := #( #primitiveAdd #primitiveSubtract #primitiveLessThan
	                 #primitiveGreaterThan #primitiveLessOrEqual
	                 #primitiveGreaterOrEqual #primitiveEqual "#primitiveNotEqual"
	                 #primitiveMultiply "#primitiveDivide" "primitiveAt" "primitiveNew
	                 primitiveNewWithArg" ).

	"bytecodePrimAdd duplicateTopBytecode 
	sendLiteralSelector0ArgsBytecode shortConditionalJumpTrue 
	  pushConstantTrueBytecode
	 "
	bytecodes := #( 
		"pushReceiverVariableBytecode"
			0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
		"pushLiteralVariable16CasesBytecode"
			16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 
		"pushLiteralConstantBytecode"
			32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
		"pushTemporaryVariableBytecode"
			64 65 66 67 68 69 70 71 72 73 74 75
		"pushReceiverBytecode"
			76
		"pushConstantTrue/False/Nil/Zero/OneBytecode"
			77 78 79 80 81
		"duplicateTop"
			83
		"returnReceiver/True/False/Nil"
			88 89 90 91
		"return top from method/nil from block/top from block" 
			92 93 94
		"Nop"
			95
		"Special sends (+)"
			96  97  98  99 100 101 102 103 104 105
			106 107 108 109 110 111 112 113 114 115
			116 117 "118 120" 121 122 123 124 125 126 127
		"sendLiteralSelector0ArgsBytecode"
			128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
		"sendLiteralSelector1ArgBytecode"
			144 145 146 147 148 149 150 150 151 152 153 154 155 156 157 158 159
		"sendLiteralSelector2ArgsBytecode"
			160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
		"shortUnconditionalJump"
			176 177 178 179 180 181 182 183
		"shortConditionalJumpTrue"
			184 185 186 187 188 189 190 191
		"shortConditionalJumpFalse"
			192 193 194 195 196 197 198 199
		"store and pop into inst var"
			"200 201 202 203 204 205 206 207"
		"store and pop into temp"
			208 209 210 211 212 213 214 215
		"popStackBytecode"
			216
		"extension bytecodes"
			224 "225 left out because it cannot be staged"
		"extended receiver/literal/temporary/constant variable bytecode"
			"226 left out because it reads contexts..."
			227 228 229 232 233
		"extended send"
			234
		"extended conditional jump"
			237 238 239
		"extended store (and pop) temporary"
			242 245
		"remote temp bytecodes"
			251
			"252 253 left out because of write barrier"
	).

	(self fromInterpreterClass: CogVMSimulatorLSB)
		doFailOnFirst;
		selectPrimitives: [ :e | primitives includes: e selector ];
		selectBytecodes: [ :e | bytecodes includes: e bytecodeNumberStart ];
		targetClass: DruidJIT;
		targetSuperclass: StackToRegisterMappingCogit;
		build
]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> addBytecodes [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	| bytecodes |
	bytecodes := (self interpreterBytecodeTable
		             reject: [ :e | e isNil ])
		flatCollect: [ :e |
		             self newBytecodes: (interpreterClass lookupSelector: e) ].

	bytecodes
		select: [ :e | self isSelectedBytecode: e ]
		thenDo: [ :e | self bytecodes add: e ].

	self fillBytecodesTableHoles
]

{ #category : #adding }
DRInterpreterToCompiler >> addMandatoryPrimitives [

	self interpreterPrimitiveTable do: [ :selector |
		(selector isNumber or: [
			 self primitives anySatisfy: [ :e | e sourceSelector = selector ] ])
			ifFalse: [
				| method |
				method := self interpreterClass lookupSelector: selector.
				(self isMaycallbackPrimitive: method) ifTrue: [
					self primitives add: (self newNonImplementedPrimitive: method) ] ] ]
]

{ #category : #adding }
DRInterpreterToCompiler >> addPrimitive: primitiveMethod [

	(self isValidPrimitiveName: primitiveMethod) ifFalse: [ 1 halt ].

	[ self primitives add: (self newPrimitive: primitiveMethod) ]
		on: Error
		do: [ :ex |
			failFirst ifTrue: [ ex pass ].
			self
				failedPrimitivesAt: ex
				add: primitiveMethod selector ]
]

{ #category : #adding }
DRInterpreterToCompiler >> addPrimitives [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	self addSelectedPrimitives.
	self addMandatoryPrimitives
]

{ #category : #adding }
DRInterpreterToCompiler >> addSelectedPrimitives [

	(self collectPrimitives: self interpreterPrimitiveTable) do: [ :prim |
		self addPrimitive: prim ]
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> build [
	"Answer a <DRCompilationUnit>, useful for model manipulation before code dumping"

	self addPrimitives.
	self addBytecodes.
	^ self compilationUnit compileUsing: self
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> buildAndCompileIn: aClass [
	"Generate and install the receiver's primitives and bytecodes into JIT compiler aClass"

	self compilationUnit targetClass: (self environmentAt: aClass).
	self build
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> buildAndCompileIn: aClass superclass: aSuperclass [
	"Generate and install the receiver's primitives and bytecodes into JIT compiler aClass"

	self compilationUnit 
		targetSuperclass: aSuperclass;
		targetClass: (self environmentAt: aClass).
	self build
]

{ #category : #adding }
DRInterpreterToCompiler >> bytecodeGroupOf: aSelector [

	self interpreterBytecodeTableArray do: [ :bytecodeSizeGroup |
		bytecodeSizeGroup second do: [ :bytecodeSpec |
			(bytecodeSpec includes: aSelector) ifTrue: [
				^ { bytecodeSizeGroup first } , bytecodeSpec ] ] ].

	self error: 'Bytecode selector not found'
]

{ #category : #constants }
DRInterpreterToCompiler >> bytecodeTable [
	"Answer a <Collection> containing <Array> with each receiver bytecode's specification" 
	
	^ self compilationUnit bytecodeTable
]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> bytecodes [

	^ self compilationUnit bytecodes
]

{ #category : #accessing }
DRInterpreterToCompiler >> bytecodes: aCollection [ 

	self compilationUnit bytecodes: aCollection 
]

{ #category : #adding }
DRInterpreterToCompiler >> collectBytecodes: aCollection [
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	| validSelectors |
	validSelectors := aCollection reject: [ :obj |  obj isNil ].

	^ validSelectors
		  collect: [ :selector | interpreterClass lookupSelector: selector ]
		  thenSelect: [ :method | self isSelectedBytecode: method ]
]

{ #category : #adding }
DRInterpreterToCompiler >> collectPrimitives: aCollection [
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	| validSelectors |
	validSelectors := aCollection reject: [ :obj | obj isNumber or: [ obj isNil ] ].

	^ validSelectors
		  collect: [ :selector | interpreterClass lookupSelector: selector ]
		  thenSelect: [ :method | self isSelectedPrimitive: method ]
]

{ #category : #initialization }
DRInterpreterToCompiler >> doFailOnFirst [

	failFirst := true
]

{ #category : #adding }
DRInterpreterToCompiler >> errors [

	^ errors
		ifNil: [
			errors :=
				PluggableDictionary new
					equalBlock: [ :a :b | a class == b class ];
					hashBlock: [ :a | a class identityHash ];
					yourself 	]
]

{ #category : #adding }
DRInterpreterToCompiler >> failedBytecodesAt: anException add: aString [

	self errors
		at: anException
		ifPresent: [ : fbs | fbs add: aString ]
		ifAbsent: [ self errors 
			at: anException 
			put: (OrderedCollection with: aString) ]
]

{ #category : #adding }
DRInterpreterToCompiler >> failedPrimitivesAt: anException add: aString [

	"Put all errors in a single place"
	self failedBytecodesAt:  anException add: aString
]

{ #category : #bytecodes }
DRInterpreterToCompiler >> fillBytecodesTableHoles [

	self interpreterBytecodeTableArray do: [ :entry |
		| bytesSize bytecodeNumberStart bytecodeNumberEnd |
		bytesSize := entry first.
		bytecodeNumberStart := entry second first first.
		bytecodeNumberEnd := entry second last second.

		self
			fillBytecodesTableHolesWithSize: bytesSize
			from: bytecodeNumberStart
			to: bytecodeNumberEnd ]
]

{ #category : #bytecodes }
DRInterpreterToCompiler >> fillBytecodesTableHolesWithSize: bytesSize from: bytecodeNumberStart to: bytecodeNumberEnd [

	self bytecodes
		detect: [ :bytecodeObject |
			bytecodeObject bytecodeNumberStart
				between: bytecodeNumberStart
				and: bytecodeNumberEnd ]
		ifFound: [ :bytecodeObject | "We need to cut the hole""Check if the cut is at the beginning"
			bytecodeObject bytecodeNumberStart <= bytecodeNumberStart ifTrue: [
				^ self
					  fillBytecodesTableHolesWithSize: bytesSize
					  from: bytecodeObject bytecodeNumberEnd + 1
					  to: bytecodeNumberEnd ].

			"Check if the cut is at the end"
			bytecodeObject bytecodeNumberEnd >= bytecodeNumberEnd ifTrue: [
				^ self
					  fillBytecodesTableHolesWithSize: bytesSize
					  from: bytecodeNumberStart
					  to: bytecodeObject bytecodeNumberStart - 1 ].

			"Cut in the middle"
			self
				fillBytecodesTableHolesWithSize: bytesSize
				from: bytecodeNumberStart
				to: bytecodeObject bytecodeNumberStart - 1.
			self
				fillBytecodesTableHolesWithSize: bytesSize
				from: bytecodeObject bytecodeNumberEnd + 1
				to: bytecodeNumberEnd ]
		ifNone: [
			self bytecodes add: (DRBytecodeObject new
					 bytecodeSize: bytesSize;
					 bytecodeNumberStart: bytecodeNumberStart;
					 bytecodeNumberEnd: bytecodeNumberEnd;
					 supported: false;
					 yourself) ]
]

{ #category : #private }
DRInterpreterToCompiler >> handleErrorDuring: aFullBlockClosure [

	aFullBlockClosure
		on: Error
		do: [ :ex |
			failFirst ifTrue: [ ex pass ].
			self failedBytecodesAt: ex add: ex asString ]
]

{ #category : #'accessing - object memory' }
DRInterpreterToCompiler >> initializationOptions [

	^ {
		#BytesPerWord . 8 .
		#ObjectMemory .  memory class name
		}
]

{ #category : #initialization }
DRInterpreterToCompiler >> initialize [ 

	super initialize.
	primitiveSelector := [ : arg | true ].
	bytecodeSelector := [ : arg | false ].
	failFirst := false.
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> interpreterBytecodeTable [
	"Answer a <Collection> of <Symbol> each one representing a selector of the receiver's bytecode table"

	^ interpreterClass bytecodeTable copyWithoutDuplicates
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> interpreterBytecodeTableArray [

	^ interpreterClass bytecodeTableArray
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass [

	^ interpreterClass
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass: anObject [

	interpreterClass := anObject.
	anObject initializePrimitiveTable.
	anObject initializeBytecodeTable.

]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> interpreterPrimitiveTable [
	"Answer a <Collection> of <Symbol> each one representing a selector (e.g. #primitiveEqual) of the receiver's primitive table"

	^ interpreterClass primitiveTable
]

{ #category : #testing }
DRInterpreterToCompiler >> isExtendedBytecode: aSelector [
	" Answer true if bytecode described by aSelector is extended "
	
	^ self interpreterBytecodeTable 
]

{ #category : #testing }
DRInterpreterToCompiler >> isMaycallbackPrimitive: primitiveMethod [

	^ primitiveMethod hasPragmaNamed: #maycallback
]

{ #category : #adding }
DRInterpreterToCompiler >> isSelectedBytecode: obj [

	^ bytecodeSelector value: obj
]

{ #category : #adding }
DRInterpreterToCompiler >> isSelectedPrimitive: obj [

	^ primitiveSelector value: obj
]

{ #category : #testing }
DRInterpreterToCompiler >> isValidPrimitiveName: aString [

	^ aString isNumber not
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> newBytecodeCompiler [

	^ (DRBytecodeCompilerCompiler forInterpreter: self newInterpreter)
		compilerClass: self targetClass;
		yourself
]

{ #category : #adding }
DRInterpreterToCompiler >> newBytecodes: bytecodeMethod [

	| bytecodeGroup start end needsSuffix |
	bytecodeGroup := self bytecodeGroupOf: bytecodeMethod selector.

	start := bytecodeGroup second.
	end := bytecodeGroup third isNumber
		       ifTrue: [ bytecodeGroup third ]
		       ifFalse: [ bytecodeGroup second ].

	needsSuffix := start ~= end.
	^ (start to: end) collect: [ :e | | suffix |
		  suffix := needsSuffix
			            ifTrue: [ e - start ]
			            ifFalse: [ '' ].
		  DRBytecodeObject new
			  bytecodeSize: bytecodeGroup first;
			  bytecodeNumberStart: e;
			  bytecodeNumberEnd: e;
			  targetSelector: ('gen_' , bytecodeMethod selector capitalized , suffix asString) asSymbol;
			  sourceMethod: bytecodeMethod;
			  yourself ]
]

{ #category : #'instance creation' }
DRInterpreterToCompiler >> newInterpreter [
	" Answer an <AbstractInterpreter|StackInterpreter> from the receiver's interpreter builder "

	memory := self newMemory.

	^ interpreterClass basicNew
		  objectMemory: memory;
		  basicInitialize
]

{ #category : #'accessing - object memory' }
DRInterpreterToCompiler >> newMemory [
	" Answer a memory manager instance. Currently a subclass of <SpurMemoryManager> "

	^ Spur64BitCoMemoryManager simulatorClass new
]

{ #category : #adding }
DRInterpreterToCompiler >> newNonImplementedPrimitive: interpreterPrimitiveMethod [

	| selector |
	selector := interpreterPrimitiveMethod selector.

	self flag: #TODO. "Duplicated code with newPrimitive:"

	^ DRJITPrimitiveObject new
		  primitiveNumber: (self primitiveNumberOf: selector);
		  sourceMethod: interpreterPrimitiveMethod;
		  argumentCount: -1;
		  mayCallback:
			  (self isMaycallbackPrimitive: interpreterPrimitiveMethod);
		  supported: false;
		  yourself
]

{ #category : #'instance creation' }
DRInterpreterToCompiler >> newPrimitive: primitiveMethod [
	" Answer a new primitive object with its metadata and CFG generated "

	| selector primCompiler |
	primCompiler := self newPrimitiveCompiler.
	selector := primitiveMethod selector.
	^ DRJITPrimitiveObject new
		  primitiveNumber: (self primitiveNumberOf: selector);
		  sourceMethod: primitiveMethod;
		  argumentCount:
			  (primCompiler numberOfArgumentsForMethod: primitiveMethod);
		  mayCallback: (self isMaycallbackPrimitive: primitiveMethod);
		  yourself
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> newPrimitiveCompiler [
	"Answer a Druid compiler configured for the receiver's interpreter builder"

	^ (DRPrimitiveCompilerCompiler forInterpreter: self newInterpreter)
			compilerClass: self targetClass;
			yourself
]

{ #category : #adding }
DRInterpreterToCompiler >> primitiveNumberOf: aSelector [
	" Answer a <Number> representing the primitive number of aSelector "

	| primNumber |

	primNumber := self interpreterPrimitiveTable indexOf: aSelector.
	^ self interpreterPrimitiveTable first = 0
		ifTrue: [ primNumber - 1 ]
		ifFalse: [ primNumber ]
]

{ #category : #helpers }
DRInterpreterToCompiler >> primitiveTable [
	"Answer a <Collection> with the JITed primitives"

	^ compilationUnit primitiveTable
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitives [

	^ self compilationUnit primitives
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitives: aCollection [
	" Add a <Collection> of primitive selectors to the receiver's compilation unit "

	self compilationUnit primitives: aCollection asOrderedCollection.
	^ self compilationUnit primitives
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> selectBytecodes: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing bytecode methods and matching conditions in selectBlock "

	bytecodeSelector := selectBlock

]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> selectPrimitives: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing primitive methods and matching conditions in selectBlock "

	primitiveSelector := selectBlock

]
