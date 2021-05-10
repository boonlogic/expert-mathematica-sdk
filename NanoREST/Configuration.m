(* Wolfram Language Package *)

BeginPackage["NanoREST`"]
(* Exported symbols added here with SymbolName::usage *)  

(* CONFIGURATION *)
GetConfig::usage="GetConfig[NanoHandle] gets the nano configuration"

ConfigureNano::usage="ConfigureNano[NanoHandle,NumericFormat,FeatureCount] sets a config with the given parameters
\nConfigureNano[NanoHandle,ConfigJSON] posts the JSON string to set the cluster parameters"

(* CLUSTER *)
AutotuneConfig::usage="AutotuneConfig[NanoHandle] autotunes the min, max, and percent variation for the given data"

GetAutotuneArray::usage="GetAutotuneArray[NanoHandle] gets the percent variations and cluster sizes calculated from autotuning"

LearningQ::usage="Learning[NanoHandle] checks whether learning is on"

SetLearningStatus::usage="SetLearningStatus[NanoHandle,Status] set the learning to true if status is true and false if status is false"

SetRootCauseEnabled::usage="SetRootCauseEnabled[NanoHandle,Status] sets the root cause ghost clusters to be on or off"

(*Options*)
MinVals::usage=""
MaxVals::usage=""
PercentVariation::usage=""
StreamingWindow::usage=""
Weights::usage=""
Labels::usage=""
NanoAccuracy::usage=""
ClusterMode::usage=""
AutotuneByFeature::usage="option for autotuning min/max by column or overall"
AutotunePV::usage="option for autotuning the percent variation"
AutotuneRange::usage="option to autotune the min and max values"
AutotuneExcludes::usage="columns to exclude from autotuning when autotuning by feature"
AutotuneMaxClusters::usage=""
StreamAutotune::usage=""
StreamGraduation::usage=""
StreamMaxClusters::usage=""
StreamMaxSamples::usage=""
StreamRateDenominator::usage=""
StreamRateNumerator::usage=""
StreamBufferLength::usage=""

Begin["`Private`"] (* Begin Private Context *) 

(******** CONFIGURATION *********)
GenerateClusterConfig[numericFormat_,minVals_,maxVals_,weights_,labels_,percentVariation_,accuracy_,streamingWindow_,clusterMode_,
	byFeature_,autotunePV_,autotuneRange_,exclusions_,maxClusters_,
	enableAutotune_,learningGraduation_,learningMaxClusters_,learningMaxSamples_,learningDenominator_,learningNumerator_,bufferLength_]:=
	ExportString[{
		"accuracy"->accuracy,
		"autoTuning"->{
			"autoTuneByFeature"->byFeature,
			"autoTunePV"->autotunePV,
			"autoTuneRange"->autotuneRange,
			"exclusions"->exclusions,
			"maxClusters"->maxClusters
		},
		"clusterMode"->clusterMode,
		"features"->Table[Drop[{
			"minVal"->minVals[[i]],
			"maxVal"->maxVals[[i]],
			"weight"->weights[[i]],
			"label"->labels[[i]]},
			If[labels[[i]]=="",-1,0]],{i,1,Length[minVals]}],
		"numericFormat"->numericFormat,
		"percentVariation"->percentVariation,
		"streaming"->{
			"enableAutoTuning"->enableAutotune,
			"learningGraduation"->learningGraduation,
			"learningMaxClusters"->learningMaxClusters,
			"learningMaxSamples"->learningMaxSamples,
			"learningRateDenominator"->learningDenominator,
			"learningRateNumerator"->learningNumerator,
			"samplesToBuffer"->bufferLength
		},
		"streamingWindowSize"->streamingWindow},
	"JSON"]
	

Options[ConfigureNano]={MinVals->0,MaxVals->10,PercentVariation->0.05,StreamingWindow->1,Weights->1,Labels->"",NanoAccuracy->0.99,ClusterMode->"batch",
	AutotuneByFeature->True,AutotunePV->True,AutotuneRange->True,AutotuneExcludes->{},AutotuneMaxClusters->1000,
	StreamAutotune->True,StreamGraduation->True,StreamMaxClusters->1000,StreamMaxSamples->10^6,StreamRateDenominator->10000,StreamRateNumerator->10,StreamBufferLength->10000};
	
ConfigureNano[NanoHandle_,NumericFormat_,FeatureCount_,OptionsPattern[]]:=Module[{req,RetVal,config,mins,maxes,weights={},labels},
	
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];

	If[Depth[OptionValue[MinVals]]==1,mins=ConstantArray[OptionValue[MinVals],FeatureCount],mins=OptionValue[MinVals]];
	If[Depth[OptionValue[MaxVals]]==1,maxes=ConstantArray[OptionValue[MaxVals],FeatureCount],maxes=OptionValue[MaxVals]];
	If[Length[maxes]!=Length[mins]||Length[mins]!=FeatureCount,Message[NanoError::length];Return[];];
	If[OptionValue[Weights]===1,
		weights=ConstantArray[1,FeatureCount],
		
		If[Length[OptionValue[Weights]]!=FeatureCount,
			Message[NanoError::length];Return[]
		];
		weights=OptionValue[Weights]
	];
	
	If[OptionValue[Labels]==="",
		labels=ConstantArray["",FeatureCount],
		
		If[Length[OptionValue[Labels]]!=FeatureCount,
			Message[NanoError::length];Return[]
		];
		labels=OptionValue[Labels]
	];
	
	If[MemberQ[{True,False},OptionValue[AutotuneByFeature]]==False,
		Message[InvalidOption::option,ToString[OptionValue[AutotuneByFeature]],ToString[AutotuneByFeature]];
		Return[]
	];
	If[MemberQ[{True,False},OptionValue[AutotunePV]]==False,
		Message[InvalidOption::option,ToString[OptionValue[AutotunePV]],ToString[AutotunePV]];
		Return[]
	];
	If[MemberQ[{True,False},OptionValue[AutotuneRange]]==False,
		Message[InvalidOption::option,ToString[OptionValue[AutotuneRange]],ToString[AutotuneRange]];
		Return[]
	];
	
	If[MemberQ[{True,False},OptionValue[StreamAutotune]]==False,
		Message[InvalidOption::option,ToString[OptionValue[StreamAutotune]],ToString[StreamAutotune]];
		Return[]
	];
	
	If[MemberQ[{True,False},OptionValue[StreamGraduation]]==False,
		Message[InvalidOption::option,ToString[OptionValue[StreamGraduation]],ToString[StreamGraduation]];
		Return[]
	];
	
	config=GenerateClusterConfig[NumericFormat,mins,maxes,weights,labels,OptionValue[PercentVariation],OptionValue[NanoAccuracy],OptionValue[StreamingWindow],OptionValue[ClusterMode],
	OptionValue[AutotuneByFeature],OptionValue[AutotunePV],OptionValue[AutotuneRange],OptionValue[AutotuneExcludes],OptionValue[AutotuneMaxClusters],
	OptionValue[StreamAutotune],OptionValue[StreamGraduation],OptionValue[StreamMaxClusters],OptionValue[StreamMaxSamples],OptionValue[StreamRateDenominator],OptionValue[StreamRateNumerator],OptionValue[StreamBufferLength]];
	
	req = HTTPRequest[NanoHandle["url"]<>"clusterConfig/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]},
	"Body"->config|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
	];
]

ConfigureNano[NanoHandle_,config_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[!AssociationQ[config],Message[NanoError::general,"Config is not a valid format. Must be an association"];Return[]];
	
	req = HTTPRequest[NanoHandle["url"]<>"clusterConfig/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]},
	"Body"->ExportString[config,"RawJSON"]|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
	];
]

(* Get the cluster config information *)
GetConfig[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[NanoHandle["url"]<>"clusterConfig/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"GET",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	Return[ImportString[RetVal[[2]],"RawJSON"]];
]

(* Get the cluster config information *)
GetAutotuneArray[NanoHandle_]:=Module[{req,RetVal,results},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[NanoHandle["url"]<>"autotuneArray/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"GET",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	results=ImportString[RetVal[[2]],"RawJSON"];
	Return[Transpose[Values[results]]];
]

(* Autotunes the cluster config *)
AutotuneConfig[NanoHandle_]:=Module[{req,url,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	
	url=NanoHandle["url"]<>"autoTune/"<>NanoHandle["instance"]
	<>"?api-tenant="<>NanoHandle["api-tenant"];
	req = HTTPRequest[url, 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	Return[];
]

LearningQ[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[NanoHandle["url"]<>"learning/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"],
	<|
	"Method"->"GET",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal=URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	If[ImportString[RetVal[[2]],"RawJSON"]===True,Return[True],Return[False]]
]

SetLearningStatus[NanoHandle_,Status_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[ToString[Status]!=True && ToString[Status]!=False, Message[MissingParameter::argerr,Status];Return[]];
	req = HTTPRequest[NanoHandle["url"]
		<>"learning/"<>NanoHandle["instance"]
		<>"?enable="<>ToLowerCase[ToString[Status]]
		<>"&api-tenant="<>NanoHandle["api-tenant"],
	<|
	"Method"->"POST",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal=URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	Return[];
]

SetRootCauseEnabled[NanoHandle_,Status_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[ToString[Status]!=True && ToString[Status]!=False, Message[MissingParameter::argerr,Status];Return[]];
	req = HTTPRequest[NanoHandle["url"]
		<>"rootCause/"<>NanoHandle["instance"]
		<>"?enable="<>ToLowerCase[ToString[Status]]
		<>"&api-tenant="<>NanoHandle["api-tenant"],
	<|
	"Method"->"POST",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal=URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	Return[];
]

End[] (* End Private Context *)

Protect[GetConfig]
Protect[ConfigureNano]
Protect[AutotuneConfig]
Protect[GetAutotuneArray]

Protect[LearningQ]
Protect[SetLearningStatus]
Protect[SetRootCauseEnabled]

EndPackage[]