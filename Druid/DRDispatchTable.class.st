"
Implements a model for a dispatch table which provides:

- Specifying a `Collection` of primitives to work with: 
  - `DRDispatchTable>>#primitives:`
  - Primitives from a ""source"" class `interpreterClass:` and `DRDispatchTable>>addFromProtocol:` 
- A protocol to search and filter primitives:
  - `allSelectorsMatching:` , `allMethodsMatching:`.
- Generation of the table in a class passed as parameter. `DRDispatchTable>>#generateInterpreterTableIn:` 

## Filtering

To filter primitives by name, one could specify the method of matching, currently supported:

- #beginsWith: _(default)_
- #matchesRegex:

## Examples

```smalltalk

| dTable |
""Instantiate a dispatch table for primitives in the Stack Interpreter""
dTable := DRDispatchTable for: StackInterpreterPrimitives.

""Query and get a Collection with all the primitives starting with 'prim'""
dTable selectorsMatching: 'prim'.

""Query primitives ending with 'Add'""
dTable useRegEx.
dTable selectorsMatching: '.*Add'.

""Add primitives to the receiver from a protocol""
dTable addFromProtocol: 'arithmetic float primitives'.
dTable primitives.

```


Internal Representation and Key Implementation Points.

    Instance Variables
	allowDuplicates:		<Object>
	excludeObjectClass:		<Object>
	interpreterClass:		<Object>
	matchMethod:		<Object>
	primitives:		<Object>
	scanSuperclasses:		<Object>


    Implementation Points
"
Class {
	#name : 'DRDispatchTable',
	#superclass : 'Object',
	#instVars : [
		'interpreterClass'
	],
	#category : 'Druid-CompilerBuilder',
	#package : 'Druid',
	#tag : 'CompilerBuilder'
}

{ #category : 'examples' }
DRDispatchTable class >> example [

	| dispatchTable |

	dispatchTable := DRDispatchTable for: StackInterpreterPrimitives. "InterpreterPrimitives"
	dispatchTable := DRDispatchTable for: StackInterpreterPrimitives.
	dispatchTable := DRDispatchTable for: StackInterpreter.
	"dispatchTable := DRDispatchTable for: CoInterpreter." 
	dispatchTable addFromSelectorPattern: 'prim'.
	dispatchTable addFromProtocol: ''.
	dispatchTable addFromProtocol: 'arithmetic float primitives'.
	dispatchTable addFromProtocols: #(
		'arithmetic float primitives' 
		'control primitives').
	" Obtain the Dictionary of primitives "
	dispatchTable methods.
	dispatchTable generateInterpreterTableIn: #TargetClass.
]

{ #category : 'instance creation' }
DRDispatchTable class >> for: anInterpreterClass [

	^ self new
		initializeFor: anInterpreterClass;
		yourself
]

{ #category : 'accessing' }
DRDispatchTable >> addFromProtocol: aString [
	" Add all methods from the protocol named aString in the receiver's interpreter classs"

	aString ifEmpty: [ ^ self ].
	(self interpreterProtocol hasProtocolNamed: aString)
		ifTrue: [ self methods addAll: (self interpreterProtocol methodsInProtocolNamed: aString) ]
]

{ #category : 'accessing' }
DRDispatchTable >> addFromProtocols: aCollectionOfProtocolNames [
	" Add all methods from the protocol named aString in the receiver's interpreter classs"

	aCollectionOfProtocolNames
		do: [ : protocolName | self addFromProtocol: protocolName ]
		displayingProgress: [ : name | 'Adding primitives in ' , name ]
]

{ #category : 'accessing' }
DRDispatchTable >> generateInterpreterTableIn: aClassName [

	self shouldBeImplemented
]

{ #category : 'initialization' }
DRDispatchTable >> initializeFor: anInterpreterClass [

	interpreterClass := anInterpreterClass
]

{ #category : 'accessing' }
DRDispatchTable >> interpreterClass [
	^ interpreterClass
]

{ #category : 'private' }
DRDispatchTable >> interpreterProtocol [
	" Answer the <ProtocolOrganizer> for the receiver's interpreter "

	^ self targetClass organization protocolOrganizer
]
