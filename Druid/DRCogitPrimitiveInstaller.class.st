"
This class can take a `DRCompilationUnit` as input (using `primitiveTableMethodFrom:`) and install methods in a JIT compiler class (using `installAllMethodsOn:`).

Methods starting with `build` answer Pharo Code Generator node instances, used to build the methods in the JIT compiler class.

"
Class {
	#name : #DRCogitPrimitiveInstaller,
	#superclass : #Object,
	#instVars : [
		'primitiveSpec',
		'primTableNode'
	],
	#category : #'Druid-InterpreterBuilder'
}
