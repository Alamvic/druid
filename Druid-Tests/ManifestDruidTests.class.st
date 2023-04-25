"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestDruidTests,
	#superclass : #PackageManifest,
	#category : #'Druid-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestDruidTests class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#DruidTestRTLCompiler)) #'2023-04-25T23:30:21.589887+02:00') )
]
