Class {
	#name : #DRCogitPrimitiveDumper,
	#superclass : #Object,
	#instVars : [
		'primitiveSpec'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRCogitPrimitiveDumper class >> dumpFrom: aDRCompilationUnit [ 
	"Answer a <String> method representation of aDRCompilationUnit"

	^ self new
		primitiveSpec: aDRCompilationUnit;
		dump
]

{ #category : #generation }
DRCogitPrimitiveDumper >> dump [

	^ String streamContents: [ : stream |
		stream
			<< 'initializePrimitiveTable';
			cr; crtab;
			<< self dumpHeader;
			<< self dumpSpec;
			<< $);
			cr ]
]

{ #category : #dumping }
DRCogitPrimitiveDumper >> dump: aCollection on: aWriteStream [

	aWriteStream
		<< $(;
		<< aCollection first asString;
		space;
		<< aCollection second asString.
	aCollection size = 3
		ifTrue: [
			aWriteStream
				space;
				<< aCollection third asString].
	aWriteStream
		<< $);
		crtab
]

{ #category : #generation }
DRCogitPrimitiveDumper >> dumpHeader [
	"Answer a <String> with necessary initialization needed by Cogit before configuring the primitive table"

	^ String streamContents: [ : stream |
		stream
			crtab;
			<< '	MaxCompiledPrimitiveIndex := self objectRepresentationClass wordSize = 8
										ifTrue: [659]
										ifFalse: [222].
		primitiveTable := CArrayAccessor on: (Array new: MaxCompiledPrimitiveIndex + 1).
		self table: primitiveTable from: #(' ]
]

{ #category : #dumping }
DRCogitPrimitiveDumper >> dumpSpec [

	^ String streamContents: [ : stream |
		 self initPrimitiveTable do: [ : primSpecEntry |
			self dump: primSpecEntry on: stream  ] ]
]

{ #category : #initialization }
DRCogitPrimitiveDumper >> initPrimitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec initPrimitiveTable
]

{ #category : #accessing }
DRCogitPrimitiveDumper >> primitiveSpec [

	^ primitiveSpec
]

{ #category : #accessing }
DRCogitPrimitiveDumper >> primitiveSpec: anObject [

	primitiveSpec := anObject
]
