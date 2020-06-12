(* Nano v3.1 MMA interface *) 

BeginPackage["NanoREST`"]
Unprotect["NanoREST`*"]
ClearAll["NanoREST`*"]
ClearAll["NanoREST`Protected`*"]

(* GENERAL *)
GetVersion::usage="GetVersion[] returns the nano version running"

(* INSTANCES *)
OpenNano::usage="OpenNano[Label] starts a nano using the default user's credentials and returns the NanoHandle
\nOpenNano[Label,User] starts a nano with the given label using the user's credentials and returns the NanoHandle"

CloseNano::usage="CloseNano[NanoHandle] stops the nano associated with the NanoHandle and closes the connection"

NanoList::usage="NanoList[NanoHandle] returns a list names for the allocated nanos"

ExistsQ::usage="ExistsQ[NanoHandle] tests to see whether the given nanohandle still poitns to an active nano"

SaveNano::usage="SaveNano[NanoHandle,Filename] saves the nano in a .bn file"

LoadNano::usage=""

(* CONFIGURATION *)
GetConfig::usage="GetConfig[NanoHandle] gets the nano configuration"

ConfigureNano::usage="ConfigureNano[NanoHandle,NumericFormat,FeatureCount] sets a config with the given parameters
\nConfigureNano[NanoHandle,ConfigJSON] posts the JSON string to set the cluster parameters"

(* CLUSTER *)
AutotuneConfig::usage="AutotuneConfig[NanoHandle] autotunes the min, max, and percent variation for the given data"

LearningQ::usage="Learning[NanoHandle] checks whether learning is on"

SetLearningStatus::usage="SetLearningStatus[NanoHandle,Status] set the learning to true if status is true and false if status is false"

LoadData::usage="LoadData[NanoHandle,data] posts the data to be clustered"

RunNano::usage="RunNano[NanoHandle] clusters the data"

RunStreamingData::usage="RunStreamingData[NanoHandle, data] pushes data through the nano and autotunes in the process"

GetBufferStatus::usage="GetBufferStatus[NanoHandle] returns information on the buffer"

GetNanoStatus::usage="GetNanoStatus[NanoHandle] returns statistics on the clusters"

GetNanoResults::usage="GetNanoResults[NanoHandle] returns the clustering results"

(* OTHER *)
GenerateRandomPatternNative::usage="GenerateRandomPatternNative[PatternLength,MaxVal] generates a random pattern of integers with values from 0 to max with length PatternLength
\nGenerateRandomPatternNative[MaxVal] generates a random pattern of integers with values from 0 to the corresponding max in each column"

GenerateRandomPatternInt::usage="GenerateRandomPatternInt[PatternLength,MinVal,MaxVal] generates a random pattern of integers with values from min to max with length PatternLength
\nGenerateRandomPatternInt[MinVal,MaxVal] generates a random pattern of integers with values from the corresponding mins to maxes in each column"

GenerateRandomPatternFloat::usage="GenerateRandomPatternFloat[PatternLength,MinVal,MaxVal] generates a random pattern of floats with values from min to max with length PatternLength
\nGenerateRandomPatternFloat[MinVal,MaxVal] generates a random pattern of floats with values from the corresponding mins and maxes in each column"

ComputePercentVariation::usage="ComputePercentVariation[v1,v2,MinVal,MaxVal] calculates the percent variation between v1 and v2 using the given parameters
\nComputePercentVariation[v1,v2,MinVal,MaxVal,WeightedVals] calculates the percent variation between v1 and v2 using the given parameters and weights"

GenerateRandomVariantInt::usage="GenerateRandomVariantInt[SourcePattern,MinVal,MaxVal,PercentVariation] generates a pseudo random integer vector variant with values from min to max
\nGenerateRandomVariantInt[SourcePattern,MinVal,MaxVal,PercentVariation,Num] generates Num pseudo random integer vector variant(s) with values from min to max"

GenerateRandomVariantNative::usage="GenerateRandomVariantNative[SourcePattern,MaxVal,PercentVariation] generates a pseudo random integer vector variant with values from 0 to max
\nGenerateRandomVariantNative[SourcePattern,MaxVal,PercentVariation,Num] generates Num pseudo random integer vector variant(s) from SourcePattern with values from 0 to max"

GenerateRandomVariantFloat::usage="GenerateRandomVariantFloat[SourcePattern,MinVal,MaxVal,PercentVariation] generates a pseudo random float vector variant with values from min to max
\nGenerateRandomVariantFloat[SourcePattern,MinVal,MaxVal,PercentVariation,Num] generates Num pseudo random float vector variant(s) with values from min to max"

DecodePM::usage="DecodePM[NanoHandle] returns the pattern memory (base64) or any of the results specified"

(*Error Messages*)
NanoError::length="Lengths of unequal value"
NanoError::return="Failed with an error code of `1` and body: `2`"
InvalidParam::argerr="The first parameter must either be a list of magnitudes or a pattern length"
FileError::argerr="File not found"
FileError::argwrite="File write error"
NanoError::handle="`1` is not a valid nano reference"
InvalidOption::option="`1` is not a valid option value for `2`"
MissingParameter::argerr="`1` was not provided or was invalid"
NanoWarning::message="`1`"

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

AppendData::usage="option for combining new data to previous data not yet clustered"
GZip::usage="option for loading data in a zip format"
Results::usage="specifies which clustering results options to return"
	ID::usage="cluster index of the inferences"
	SI::usage="smoothed anomaly index ranging from 0 to 1000"
	RI::usage="anomaly index ranging from 0 to 1000"
	FI::usage="frequency index ranging from 0 to Infinity"
	DI::usage="distance index ranging from 0 to 1000"
	
	PCA::usage="three dimensional cluster distance reduction"
	clusterGrowth::usage="indexes of new clusters created"
	clusterSizes::usage="number of inferences in each cluster"
	anomalyIndexes::usage="ranges from 0 to 1000"
	distanceIndexes::usage="ranges from 0 to 1000"
	frequencyIndexes::usage="ranges from 0 to Infinity"
	totalInferences::usage="number of inferences clustered"
	averageInferenceTime::usage="time to cluster per inference"
	numClusters::usage="number of clusters created (including the zero cluster)"
	
RowSort::usage="pattern memory sorting based on distance"

Exact::usage="whether to make the variant vector exactly the given percent variation away from the source pattern"

points::usage="documentation variable"
r::usage="documentation variable"
c::usage="documentation variable"

Begin["`Private`"]

(*messageHandler = If[Last[#], Abort[]] &;
Internal`AddHandler["Message", messageHandler];*)

(* Syntax 1: GenerateRandomPatternNative[5,13] generate a random pattern of length 5 of max 13 *)
(* Syntax 2: GenerateRandomPatternNative[{17,5,30,5}] generate a random pattern of length 4 the max values given *)
GenerateRandomPatternNative[Param1_,Max_:0]:=Module[{},
	If[Depth[Param1]==1,(* this is syntax 1 *)
	Return[RandomInteger[{0,Max},Param1]]];
		
	If[Depth[Param1]!=2,Message[InvalidParam::argerr];
		Return[]
	];
	Return[RandomInteger[{0,#}]&/@Param1]
]

(* If Param1 and Param2 are lists, then those lists are used as corresponding min and max for generating the pattern*)
(* Otherwise, Param1 is the pattern length, Param2 is the min, and Param3 is the max *)
GenerateRandomPatternInt[Param1_,Param2_,Param3_:0]:=Module[{},
	If[Depth[Param1]==1,(* this is syntax 1 *)
		Return[RandomInteger[{Param2,Param3},Param1]]
	];
	If[Depth[Param1]!=2||Depth[Param1]!=2||Length[Param1]!=Length[Param2],
		Message[NanoError::length];
		Return[]
	];
	Return[RandomInteger[#]&/@Transpose[{Param1,Param2}]]
]

GenerateRandomPatternFloat[Param1_,Param2_,Param3_:0]:=Module[{},
	If[Depth[Param1]==1,(* this is syntax 1 *)
		Return[RandomReal[{Param2,Param3},Param1]]
	];
	If[Depth[Param1]!=2||Depth[Param1]!=2||Length[Param1]!=Length[Param2],
		Message[NanoError::length];
		Return[]
	];
	Return[RandomReal[#]&/@Transpose[{Param1,Param2}]]
]

(* Min and Max are either numbers or lists the same length as V1 and V2 (for variable ranges) *)
ListClip[L_,Min_, Max_]:=Table[Clip[L[[i]],{Min[[i]],Max[[i]]}],{i,1,Length[L]}]
ComputePercentVariation[V1_,V2_,Min_,Max_,WeightsIn_:1]:=Module[{PatternLength,ExpandedMinVal=Min, ExpandedMax=Max,ColPercentDifferences,ColRelativeWeights,Weights},
	If[Length[V1]!=Length[V2],
		Message[NanoError::length];
		Return[]
	];
	PatternLength=Length[V1];

	If[Depth[Min]==1, ExpandedMinVal=ConstantArray[Min,PatternLength]];
	If[Depth[Max]==1, ExpandedMax=ConstantArray[Max,PatternLength]];
	
	If[Length[ExpandedMinVal]!=Length[V2],
		Message[NanoError::length];
		Return[]
	];
	
	If[WeightsIn===1,Weights=ConstantArray[1,PatternLength],
	If[Length[WeightsIn]!=PatternLength, Message[NanoError::length];Return[]],
	Weights=WeightsIn];
	
	ColPercentDifferences=Abs[ListClip[V1,ExpandedMinVal,ExpandedMax]-ListClip[V2,ExpandedMinVal,ExpandedMax]]/(ExpandedMax-ExpandedMinVal+1);
	ColRelativeWeights=Weights/Total[Weights];
	Return[Total[N[ColPercentDifferences*ColRelativeWeights]]];
]

Options[GenerateRandomVariantFloat]={Exact->True};
(* Create a variant of SourcePattern less than or equal to the PercentVariation *)
GenerateRandomVariantFloat[SourcePattern_,Min_,Max_,PercentVariation_,Num_:1,WeightsIn_:1,opts:OptionsPattern[]]:=Module[{allVariants={},i,pv,VariationDirection,PatternLength,IndexToChange,VariantPattern,AccumulatedVariation,ExpandedMinArray = Min, ExpandedMaxArray = Max,IncrementPerFeature,Variation,VariationPerIncrement,Weights},
	If[MemberQ[{True,False},OptionValue[Exact]]==False,
		Message[NanoError::option,
		ToString[OptionValue[Exact]],ToString[Exact]];
		Return[]
	];
	
	PatternLength=Length[SourcePattern];
	If[Depth[Min]==1, ExpandedMinArray=ConstantArray[Min,PatternLength]];
	If[Depth[Max]==1, ExpandedMaxArray=ConstantArray[Max,PatternLength]];
	
	If[WeightsIn===1,
		Weights=ConstantArray[1,PatternLength],
		If[Length[WeightsIn]!=PatternLength,
			Message[NanoError::length];
			Return[]
		];
		Weights=WeightsIn
	];

	For[i=1,i<=Num,i++,
		VariantPattern=SourcePattern;
		VariationDirection=RandomChoice[{-1,1},PatternLength];
		AccumulatedVariation=0;
		pv=If[OptionValue[Exact]==True,PercentVariation,RandomReal[{0.0001,PercentVariation}]];(*Depending if you want the cloud affect or exactly the percent variation*)
		IncrementPerFeature=(ExpandedMaxArray-ExpandedMinArray+1)/1000.0; (*Increment by 100ths of a percent in each feature *)
		VariationPerIncrement=(Weights/Total[Weights])/1000.0;
		While[AccumulatedVariation<pv,
			IndexToChange=RandomInteger[{1,PatternLength}];
			Variation=VariationDirection[[IndexToChange]]*IncrementPerFeature[[IndexToChange]];
			If[ExpandedMinArray[[IndexToChange]]<= VariantPattern[[IndexToChange]]+Variation<=ExpandedMaxArray[[IndexToChange]],
				VariantPattern[[IndexToChange]]+=Variation;
				AccumulatedVariation+=VariationPerIncrement[[IndexToChange]];
			]
		];
		AppendTo[allVariants,VariantPattern];
	];
	Return[allVariants];
]

Options[GenerateRandomVariantNative]={Exact->True};
Options[GenerateRandomVariantInt]={Exact->True};
GenerateRandomVariantNative[SourcePattern_,Max_,PercentVariation_,Num_:1,Weights_:1,OptionsPattern[]]:=Round[GenerateRandomVariantFloat[SourcePattern,0,Max,PercentVariation,Num,Weights,Exact->OptionValue[Exact]]];
GenerateRandomVariantInt[SourcePattern_,Min_,Max_,PercentVariation_,Num_:1,Weights_:1,OptionsPattern[]]:=Round[GenerateRandomVariantFloat[SourcePattern,Min,Max,PercentVariation,Num,Weights,Exact->OptionValue[Exact]]]

(****** INSTANCE ******)
(* Create a nano instance *)
Options[OpenNano]={"Filename"->None,"AuthenticationPath"->FileNameJoin[{$HomeDirectory,".BoonLogic.license"}]};
OpenNano[Label_,User_:"default",OptionsPattern[]]:=Module[{cred},
	cred=Check[SetServerGlobals[User,OptionValue["AuthenticationPath"]],Return[]];
	cred["instance"]=Check[CreateInstance[cred,Label],Return[cred],NanoError::return];
	If[OptionValue["Filename"]=!="" && OptionValue["Filename"]=!=None,
		Check[LoadNano[cred,OptionValue["Filename"]],Return[cred],NanoError::return];
	];
	Return[cred]
]

SetServerGlobals[User_,AuthPath_]:=Module[{Credentials,proxy},
	Credentials=Check[Import[AuthPath,"RawJSON"],Return[]];
	If[MemberQ[Keys[Credentials],User]==False,Message[NanoError::handle,User];
	Return[]];
	If[StringContainsQ[Credentials[User,"server"],"http"],
		Credentials[User,"url"]=Credentials[User,"server"]<>"/expert/v3/";,
		Credentials[User,"url"]="http://"<>Credentials[User,"server"]<>"/expert/v3/";
	];
	If[Credentials[User,"proxy-server"]!="",
		PacletManager`UseInternetProxy[True];
		proxy=StringReplace[#,"/"->""]&/@StringSplit[Credentials[User,"proxy-server"],":"];
		PacletManager`SetInternetProxy["HTTPS",{proxy[[-2]],proxy[[-1]]}];,
		PacletManager`UseInternetProxy[False];
	];
	Credentials[User]=KeyDrop[Credentials[User],"server"]; 
	Return[Credentials[User]];
]

CreateInstance[NanoHandle_,Label_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[NanoHandle["url"]<>"nanoInstance/"<>Label<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	Return[ImportString[RetVal[[2]],"RawJSON"]["instanceID"]];
]

(* Delete specified nano instance *)
CloseNano[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[NanoHandle["url"]<>"nanoInstance/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"DELETE",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
	];
]

(* get running instances *)
NanoList[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[NanoHandle["url"]<>"nanoInstances"<>"?api-tenant="<>NanoHandle["api-tenant"], 
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

ExistsQ[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Return[False]];
	req = HTTPRequest[NanoHandle["url"]<>"nanoInstance/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"],
	<|
	"Method"->"GET",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal=URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		(*Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];*)
		Return[False];
	];
	Return[True]
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

(* Get saved version of the nano state *)
SaveNano[NanoHandle_,Filename_:""]:=Module[{res,file,req,RetVal,index=1,currentDirectory},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	currentDirectory=Directory[];
	If[Filename === "" || Filename === None,
		file="Untitled-"<>ToString[index];
		PrependTo[$Path,"."];
		While[FindFile[file]=!=$Failed,
			index++;
			file=StringDrop[file,-1]<>ToString[index];
		];
		file=file<>".bn";,
		
		(* else: filename given check for directory path *)
		file=FileNameSplit[Filename];
		If[Length[file]>1,
			SetDirectory[FileNameJoin[Drop[file,-1]]];
		];
		file=file[[-1]];
	];
	req = HTTPRequest[NanoHandle["url"]<>"snapshot/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"GET",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	res=Export[file, ImportString[RetVal[[2]],"Byte"],"Byte"];
	SetDirectory[currentDirectory];
	If[res===$Failed,
		Return[],
		Return[file];
	]
]

(* Post the saved version  *)
LoadNano[NanoHandle_,Filename_]:=Module[{req,RetVal,file,currentDirectory},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	currentDirectory=Directory[];
	file=FileNameSplit[Filename];
	If[Length[file]>1,
		SetDirectory[FileNameJoin[Drop[file,-1]]];
	];
	file=file[[-1]];
	file=FindFile[file];
	If[StringQ[file]==False,Message[FileError::argerr];Return[]]; (* if file is not a string then it is $Failed*)
	req = HTTPRequest[NanoHandle["url"]<>"snapshot/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"multipart/form-data","x-token"->NanoHandle["api-key"]},
	"Body"->{"snapshot"->File[file]}
	|>];
	RetVal= URLRead[req,{"Status","Body"}];
	SetDirectory[currentDirectory];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	Return[];
]

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

(******** CLUSTER ********)
(* Uploads the data to be clustered and returns any results specified *)
Options[LoadData]={AppendData->False,GZip->False};
LoadData[NanoHandle_,Data_,OptionsPattern[]]:=Module[{result,req,url,RetVal,t,NumericFormat,tempFile="DataToPost.bin"},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	
	If[MemberQ[{True,False},OptionValue[AppendData]]==False,
		Message[InvalidOption::option,ToString[OptionValue[AppendData]],ToString[AppendData]];
		Return[]
	];
	
	If[MemberQ[{True,False},OptionValue[GZip]]==False,
		Message[InvalidOption::option,ToString[OptionValue[GZip]],ToString[GZip]];
		Return[]
	];
	
	NumericFormat=GetConfig[NanoHandle]["numericFormat"];
	SetDirectory[$TemporaryDirectory];
	Quiet[Close[tempFile]];
	If[ToString[FindFile[tempFile]]!="$Failed",DeleteFile[tempFile]];
	result=Quiet[Export[tempFile,Flatten[Data],Which[NumericFormat=="float32","Real32",NumericFormat=="uint16","UnsignedInteger16",NumericFormat=="int16","Integer32",True,NumericFormat]]];
	If[result===$Failed,
		ResetDirectory[];
		Message[FileError::argwrite];
		Return[]
	];
	t=FindFile[tempFile];
	url=NanoHandle["url"]<>"data/"<>NanoHandle["instance"]
	<>"?runNano=false"
	<>"&fileType=raw"
	<>"&gzip="<>ToLowerCase[ToString[OptionValue[GZip]]]
	<>"&appendData="<>ToLowerCase[ToString[OptionValue[AppendData]]]
	<>"&api-tenant="<>NanoHandle["api-tenant"];

	req = HTTPRequest[url, 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"multipart/form-data","x-token"->NanoHandle["api-key"],"type"->"text/csv"},
	"Body"->{"data"->File[t]}
	|>];
	RetVal= URLRead[req,{"Status","Body"}];
	ResetDirectory[];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
	];
]

(* Uploads the data to be clustered for streaming applications *)
Options[RunStreamingData]={Results->ID,GZip->False};
RunStreamingData[NanoHandle_,Data_,OptionsPattern[]]:=Module[{result,ResultString,req,url,RetVal,t,NumericFormat,tempFile="DataToPost.bin"},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	
	If[SubsetQ[{None,All,ID,SI,RI,FI,DI},Flatten[{OptionValue[Results]}]]==False,
		Message[InvalidOption::option,ToString[OptionValue[Results]],ToString[Results]];
		Return[]
	];
	
	If[MemberQ[{True,False},OptionValue[GZip]]==False,
		Message[InvalidOption::option,ToString[OptionValue[GZip]],ToString[GZip]];
		Return[]
	];
	
	If[MemberQ[Flatten[{OptionValue[Results]}],None]==False,
		If[MemberQ[Flatten[{OptionValue[Results]}],All],
			ResultString=ToString[{ID,SI,RI,FI,DI}], (* list out all result options *)
			ResultString=ToString[OptionValue[Results]]; (* not None or All so must be a subset of result options *)
		]
	];
	
	NumericFormat=GetConfig[NanoHandle]["numericFormat"];
	SetDirectory[$TemporaryDirectory];
	Quiet[Close[tempFile]];
	If[ToString[FindFile[tempFile]]!="$Failed",DeleteFile[tempFile]];
	result=Quiet[Export[tempFile,Flatten[Data],Which[NumericFormat=="float32","Real32",NumericFormat=="uint16","UnsignedInteger16",NumericFormat=="int16","Integer32",True,NumericFormat]]];
	If[result===$Failed,
		ResetDirectory[];
		Message[FileError::argwrite];
		Return[]
	];
	t=FindFile[tempFile];
	url=NanoHandle["url"]<>"nanoRunStreaming/"<>NanoHandle["instance"]
	<>"?fileType=raw"
	<>"&gzip="<>ToLowerCase[ToString[OptionValue[GZip]]]
	<>"&results="<>StringDelete[ResultString, {" ", "{", "}","NanoREST`","Private`"}]
	<>"&api-tenant="<>NanoHandle["api-tenant"];

	req = HTTPRequest[url, 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"multipart/form-data","x-token"->NanoHandle["api-key"],"type"->"text/csv"},
	"Body"->{"data"->File[t]}
	|>];
	RetVal= URLRead[req,{"Status","Body"}];
	ResetDirectory[];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
	];
	Return[ImportString[RetVal[[2]],"RawJSON"]];
]

(* Post a nanorun using previous data and clustering parameters *)
Options[RunNano]={Results->None};
RunNano[NanoHandle_,OptionsPattern[]]:=Module[{req,url,RetVal,ResultString=""},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[SubsetQ[{None,All,ID,SI,RI,FI,DI},Flatten[{OptionValue[Results]}]]==False,
		Message[InvalidOption::option,ToString[OptionValue[Results]],ToString[Results]];
		Return[]
	];
	If[MemberQ[Flatten[{OptionValue[Results]}],None]==False,
		If[MemberQ[Flatten[{OptionValue[Results]}],All],
			ResultString=ToString[{ID,SI,RI,FI,DI}], (* list out all result options *)
			ResultString=ToString[OptionValue[Results]]; (* not None or All so must be a subset of result options *)
		]
	];
	
	url=NanoHandle["url"]<>"nanoRun/"<>NanoHandle["instance"]
	<>"?results="<>StringDelete[ResultString, {" ", "{", "}","NanoREST`","Private`"}]
	<>"&api-tenant="<>NanoHandle["api-tenant"];
	
	req = HTTPRequest[url, 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	If[ResultString==="",Return[]];
	Return[ImportString[RetVal[[2]],"RawJSON"]];
]

(* Get server version information *)
GetVersion[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[StringDrop[NanoHandle["url"],-3]<>"version"<>"?api-tenant="<>NanoHandle["api-tenant"], 
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

GetBufferStatus[NanoHandle_]:=Module[{req,RetVal},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	req = HTTPRequest[NanoHandle["url"]<>"bufferStatus/"<>NanoHandle["instance"]<>"?api-tenant="<>NanoHandle["api-tenant"], 
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

SortStatusResults[ResultString_] := Module[{new = {}, vals = {PCA,"patternMemory",clusterGrowth,clusterSizes,anomalyIndexes,frequencyIndexes,distanceIndexes,totalInferences,averageInferenceTime,numClusters,"disArray"}},
	Do[new=Join[new,Position[ResultString,i]],{i,vals}];
	new=DeleteDuplicates[ResultString[[Flatten[new]]]];
	new=StringDelete[ToString[#],{"NanoREST`"}]&/@new;
	Return[new]
]

Options[GetNanoStatus]={Results->All};
GetNanoStatus[NanoHandle_,OptionsPattern[]]:=Module[{req,url,RetVal,ResultString},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[SubsetQ[{All,PCA,"patternMemory",clusterGrowth,clusterSizes,anomalyIndexes,distanceIndexes,frequencyIndexes,totalInferences,averageInferenceTime,numClusters,"disArray"},Flatten[{OptionValue[Results]}]]==False,
		Message[InvalidOption::option,ToString[OptionValue[Results]],ToString[Results]];
		Return[]
	];
	If[MemberQ[Flatten[{OptionValue[Results]}],All],
		ResultString=ToString[{PCA,clusterGrowth,clusterSizes,anomalyIndexes,frequencyIndexes,distanceIndexes,totalInferences,numClusters}], (* list out all result options *)
		ResultString=ToString[SortStatusResults[Flatten[{OptionValue[Results]}]]]; (* not None or All so must be a subset of result options *)
	];

	
	url=NanoHandle["url"]<>"nanoStatus/"<>NanoHandle["instance"]
	<>"?results="<>StringDelete[ResultString, {" ", "{", "}","NanoREST`","Private`"}]
	<>"&api-tenant="<>NanoHandle["api-tenant"];
	
	req = HTTPRequest[url, 
	<|
	"Method" ->"GET",
	"Headers"->{"Content-Type"->"application/json","x-token"->NanoHandle["api-key"]}|>];
	RetVal= URLRead[req,{"Status","Body"}];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
		Return[];
	];
	ResultString=ImportString[RetVal[[2]],"RawJSON"];
	Return[ResultString];
]

SortResultsResults[ResultString_]:=Module[{new={},vals={ID,SI,RI,FI,DI}},
	Do[new=Join[new,Position[ResultString,i]],{i,vals}];
	Return[ToString[DeleteDuplicates[ResultString[[Flatten[new]]]]]]
]

Options[GetNanoResults]={Results->All};
GetNanoResults[NanoHandle_,OptionsPattern[]]:=Module[{req,url,RetVal,ResultString},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[SubsetQ[{All,ID,SI,RI,FI,DI,MD},Flatten[{OptionValue[Results]}]]==False,
		Message[InvalidOption::option,ToString[OptionValue[Results]],ToString[Results]];
		Return[]
	];
	If[MemberQ[Flatten[{OptionValue[Results]}],All],
		ResultString=ToString[{ID,SI,RI,FI,DI}], (* list out all result options *)
		ResultString=ToString[SortResultsResults[Flatten[{OptionValue[Results]}]]]; (* not All so must be a subset of result options *)
	];
	
	url=NanoHandle["url"]<>"nanoResults/"<>NanoHandle["instance"]
	<>"?results="<>StringDelete[ResultString, {" ", "{", "}","NanoREST`","Private`"}]
	<>"&api-tenant="<>NanoHandle["api-tenant"];
	
	req = HTTPRequest[url, 
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

Options[DecodePM]={Results->"BinaryPM",RowSort->False};
DecodePM[NanoHandle_,OptionsPattern[]]:=
	Module[{ratio,pad,pca,PM,ord,PMDecimal,numberOfEachSample,sampleSortedPM,image,temp,sourceVec,cmyk,x,y,z,xClust,yClust,min,max,sortOrder,returnList={},samples,featSig},
		If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
		If[SubsetQ[{"CMYK","SourceVector","BinaryPM","Features"},Flatten[{OptionValue[Results]}]]==False,
			Message[InvalidOption::option,ToString[OptionValue[Results]],ToString[Results]];
			Return[]
		];
		If[MemberQ[{True,False},OptionValue[RowSort]]==False,
			Message[InvalidOption::option,ToString[OptionValue[RowSort]],ToString[RowSort]];
			Return[]
		];
		
		{PM,pca,samples}=Values[KeyTake[GetNanoStatus[NanoHandle,Results->{"patternMemory",PCA,"disArray"}],{"patternMemory","PCA","disArray"}]];
		PMDecimal=ImportString[#,{"Base64","Binary"}]&/@PM;
		PM=Partition[Flatten[Reverse[IntegerDigits[#,2,8]]&/@Flatten[PMDecimal]],Length[PMDecimal[[1]]]*8];
		
		{min,max}=Transpose[Values[KeyTake[#,{"minVal","maxVal"}]]&/@(GetConfig[NanoHandle]["features"])];
		pca=Drop[pca,1];
		
		If[OptionValue[RowSort],
			{x,y,z}=Flatten[Quiet[Table[First[Position[pca,_?(#[[i]]==Max[Transpose[pca][[i]]]&),1]],{i,1,3}]]];
			xClust=Select[Complement[Range[Length[pca]],{x,y,z}],pca[[#,1]]>=pca[[#,2]]&];
			yClust=Select[Complement[Range[Length[pca]],{x,y,z}],pca[[#,1]]<pca[[#,2]]&];
			yClust=yClust[[Reverse[Ordering[pca[[yClust,3]]]]]];
			xClust=xClust[[Ordering[pca[[xClust,3]]]]];
			sortOrder=Flatten[{x,xClust,z,yClust,y}];
		];
		     
		If[MemberQ[Flatten[{OptionValue[Results]}],"BinaryPM"],
			returnList={"BinaryPM"->PM[[If[OptionValue[RowSort],sortOrder,All]]]};
		];
		
		(* if only returning binary pm, return before calculating the rest *)
		If[Flatten[{OptionValue[Results]}]==={"BinaryPM"},
			Return[Association[returnList]]
		];
		
		numberOfEachSample=BinCounts[samples,{0,Length[min]}];
		ord=TakeList[Ordering[samples],numberOfEachSample];
		sampleSortedPM=PM[[All,#]]&/@ord;
		image=Transpose[Table[If[numberOfEachSample[[i]]!=0,Count[#,0]&/@sampleSortedPM[[i]]/numberOfEachSample[[i]]*1.,ConstantArray[0,Length[sampleSortedPM[[i]]]]],{i,1,Length[numberOfEachSample]}]];
		
		If[MemberQ[Flatten[{OptionValue[Results]}],"SourceVector"],
			sourceVec=image;(*(#*(max-min)+min)&/@image;*)
			returnList=Join[returnList,{"SourceVector"->sourceVec}];
		];
		If[MemberQ[Flatten[{OptionValue[Results]}],"CMYK"],
			cmyk=Table[Join[pca[[#]],{image[[#,i]]}],{i,1,Length[image[[1]]]}]&/@Range[Length[image]];
			ratio=Clip[Round[2\[Pi]/(.1*Length[pca])],{1,\[Infinity]}];
			pad=Clip[Round[#[[2]]/ratio,#[[1]]]/#[[1]],{1,\[Infinity]}]&@(Dimensions@cmyk);
			cmyk=Flatten[ConstantArray[#,pad]&/@cmyk,1];
			returnList=Join[returnList,{"CMYK"->cmyk[[If[OptionValue[RowSort],sortOrder*pad,All]]]}];
		];
		If[MemberQ[Flatten[{OptionValue[Results]}],"Features"],
			featSig=Transpose[
				Table[
					temp=Table[HammingDistance[sampleSortedPM[[feature,cluster]],sampleSortedPM[[feature,i]]],{cluster,1,Length[sampleSortedPM[[1]]]},{i,cluster+1,Length[sampleSortedPM[[1]]]}];
					temp=Table[Join[Table[temp[[j,i-j]],{j,1,i-1}],{0},temp[[i]]],{i,1,Length[temp]}],
					{feature,1,Length[sampleSortedPM]}
				]
			];
			featSig=Table[Total[If[numberOfEachSample[[index]]==0,0,cluster[[index]]/(1.0*numberOfEachSample[[index]]*(Length[sampleSortedPM[[1]]]-1))]],{cluster,featSig},{index,1,Length[cluster]}]; (* divide by number of samples and number of other clusters (number of clusters minus one) *)
			returnList=Join[returnList,{"Features"->featSig}];
		];
		Return[Association[returnList]]
	]

(* EXAMPLE DATA - can't figure out example file storage *)

points={{1.06,1.07},{1.17,1.08},{1.05,1.48},{1.23,1.23},{1,0.97},{1.26,1.17},{1.02,1.01},{1.09,0.93},{1.03,0.98},{0.61,1.17},{1.07,1.28},{1.02,1.02},{1.02,1.12},{0.98,0.99},{0.72,1.32},{0.85,0.94},{0.51,1.07},{0.66,0.95},{0.91,0.99},{0.97,1.01},{1.15,1.15},{0.63,1.28},{0.69,0.97},{1.19,0.78},{1,1.16},{0.97,1.03},{0.82,0.54},{0.79,0.89},{0.97,1.06},{0.9,0.97},{0.97,0.53},{1.44,1.07},{1.01,1.01},{1.32,1.1},{0.96,0.84},{1,1.18},{0.67,1.25},{0.78,0.76},{1.03,0.96},{0.95,0.89},{0.62,1.25},{0.87,1.35},{0.94,0.59},{1.12,1.07},{0.91,0.88},{1.13,1.22},{0.9,1.19},{0.92,1.07},{1,1},{1.07,0.77},{1.4,1.14},{1,0.93},{1.07,0.91},{1.04,1.39},{0.98,0.77},{0.76,0.89},{1.32,1.05},{1.16,1.13},{0.53,0.99},{1.3,0.8},{1.13,1.08},{1.11,0.88},{0.83,1.03},{0.88,0.89},{1.25,0.92},{1.05,0.99},{1.07,0.93},{1.21,1.29},{1.5,1.01},{0.77,1.24},{1.1,1.01},{1.39,1.19},{0.98,1.09},{1.03,1.38},{0.97,0.85},{0.86,0.69},{0.74,1.28},{1.23,1.07},{1.01,0.91},{0.96,1.07},{1.2,1.03},{0.83,1},{0.95,1.01},{0.94,0.86},{0.94,0.9},{0.94,1.22},{1.09,0.75},{1.15,0.99},{0.85,0.95},{0.99,0.62},{0.88,1.38},{1.22,1.19},{1.08,0.67},{0.75,0.77},{0.9,0.57},{1.09,1.02},{0.67,0.92},{0.95,0.9},{0.96,0.78},{0.9,1.02},{0.94,0.96},{0.81,0.96},{0.54,0.88},{0.6,1.09},{0.79,1.25},{0.74,1.36},{1,1},{0.75,0.83},{1.23,0.75},{1.44,0.79},{1.12,1.04},{0.8,0.78},{0.93,0.98},{1.1,1},{0.79,1.18},{1,1},{0.75,0.9},{1.09,0.94},{0.92,1.06},{1.05,1.03},{0.79,0.56},{0.62,0.79},{0.98,1.32},{1.36,1.15},{0.95,1.02},{1,0.73},{0.74,1.32},{0.86,0.96},{0.95,1.02},{0.94,1.03},{1.03,0.94},{0.88,1.12},{1.26,1.14},{0.81,0.87},{1.1,1.06},{1.34,1.28},{1.03,0.97},{0.88,0.94},{0.92,1.24},{1.3,1.12},{0.62,0.98},{1.27,0.88},{0.84,0.67},{0.96,0.9},{0.96,1.12},{0.64,1.35},{0.88,1.02},{1.29,0.77},{1.17,0.6},{0.94,0.99},{0.92,0.99},{0.88,0.52},{0.53,1},{0.6,0.92},{0.75,1.04},{1.01,0.94},{0.98,0.82},{0.91,1.48},{1.44,1.08},{0.79,0.83},{0.86,0.96},{1.12,0.73},{1.35,0.88},{0.92,0.81},{1.02,1.03},{1.45,1.07},{0.95,1},{1.22,1.08},{0.9,1.32},{1.41,1.17},{0.7,1.17},{0.65,0.9},{0.6,1.25},{1,0.6},{0.78,1.15},{0.98,0.99},{1.09,1.48},{0.62,0.73},{0.77,0.88},{1.05,1.18},{0.89,1.19},{1.02,1.06},{1.04,0.96},{1.03,1.06},{0.76,0.88},{0.85,0.86},{1.46,0.85},{1.07,0.53},{1.33,0.84},{0.82,1.03},{1.35,1.28},{0.99,0.97},{0.54,0.83},{0.93,0.88},{1.41,0.95},{0.99,0.96},{1.21,0.63},{0.76,1.22},{1.39,0.91},{0.69,1},{0.92,1.21},{1.05,0.83},{0.9,1.06},{0.9,0.99},{1.35,0.86},
		{1.19,1.14},{1.3,1.24},{1.17,0.81},{0.62,1.26},{1.49,0.95},{0.64,0.91},{1.1,0.74},{0.69,1.02},{1.07,0.64},{1.16,1.28},{0.87,0.63},{1.08,1.04},{0.69,0.97},{1.24,1.32},{1.05,0.98},{0.98,1.05},{1.02,0.98},{0.79,0.88},{0.98,0.98},{1.05,1.03},{0.6,1.09},{0.66,0.92},{1.22,1.4},{1.09,1.13},{0.98,0.51},{0.65,0.95},{1,0.97},{1.33,0.92},{0.61,1.24},{0.93,0.63},{0.85,0.7},{1.13,1.24},{1.25,1.23},{0.75,1.39},{1.39,0.87},{1.02,0.99},{1.1,0.65},{0.52,1.11},{0.82,0.93},{0.95,0.93},{1.3,0.86},{1.09,0.97},{0.85,0.98},{1.18,0.91},{0.66,1.07},{0.94,0.85},{0.82,0.87},{1.2,1.07},{1.14,0.76},{1.02,0.76},{0.64,0.96},{1.18,0.56},{0.72,1.28},{1.09,1.16},{1.21,0.99},{1,1.02},{0.99,0.79},{0.65,0.76},{0.75,0.95},{1.32,1.17},{1.01,1.04},{1.03,0.65},{1.3,0.85},{1.03,0.93},{1.05,1.47},{0.96,0.98},{1.39,0.69},{0.94,1.05},{1.21,0.65},{0.51,1.02},{1.02,1.04},{1.11,1.17},{0.95,1},{1.23,1.23},{0.54,1.19},{1.18,0.85},{1.13,0.94},{0.59,1.11},{0.67,1.05},{1.2,1.38},{0.85,0.75},{0.88,1.05},{1.23,1.12},{0.92,1.29},{1.02,1},{0.75,1.03},{1.36,0.98},{1.31,1.18},{0.99,1.15},{0.89,0.56},{1.14,1.39},{1,1},{1.1,1.06},{0.69,1.11},{1.08,0.79},{1.04,1.42},{0.99,1.11},{1.06,1.22},{1.06,1.05},{0.68,1.26},{1.35,1.22},{1.02,1.01},{1.04,0.89},{1.07,0.87},{1.03,0.86},{0.99,1.16},{1.14,0.96},{0.92,1.08},{1.22,0.84},{0.54,1.11},{1.07,1.06},{1.4,1.17},{0.97,0.82},{1.15,1.14},{0.84,0.97},{0.84,1.44},{1.11,0.95},{1.17,0.89},{1.37,1.04},{1.13,1.11},{1.13,0.84},{0.68,0.82},{1,1},{0.76,0.94},{0.82,1},{1.06,1.02},{0.9,0.76},{1.08,0.96},{1.47,0.86},{0.9,1.1},{1.14,0.96},{1.2,1.38},{0.98,1.1},{0.95,0.92},{1.19,1.01},{1.06,1.05},{0.58,0.92},{0.92,0.62},{0.8,0.85},{1.24,1.09},{0.87,1.46},{1.1,1.33},{1.29,1.13},{0.98,1.17},{1.18,0.8},{1.01,1},{0.81,0.81},{1.05,0.84},{1.19,0.74},{1.01,0.9},{1.02,1.49},{0.74,0.71},{1.13,0.61},{1,0.99},{1.36,1.28},{1.04,1.19},{0.94,0.69},{1.08,1.18},{1.3,0.95},{0.93,0.75},{0.94,0.83},{1.1,1.16},{1.11,1.13},{0.88,1.19},{1.21,1.42},{1.09,0.65},{0.57,1.11},{0.58,1},{1.02,0.76},{1.1,1.33},{1.01,0.88},{1.1,0.56},{0.88,1.05},{1.16,0.58},{1.21,0.85},{1.08,0.84},{1.19,1.42},{0.65,1.2},{1.25,0.74},{1.12,0.81},{0.84,0.78},{0.88,1.35},{0.96,1.08},{0.67,0.92},{1.11,0.62},{1.13,1.21},{0.68,1.25},{1.39,1.21},{0.96,0.95},{0.97,0.95},{1.19,1.24},{0.88,0.82},{1.17,1.02},{1.33,1.24},{0.9,1.26},{1.25,0.7},{1.07,1.23},{0.84,0.69},{1.12,1.11},{1.01,0.56},{1.02,0.83},{0.98,1.44},{0.97,0.87},{1.37,1.24},
		{0.86,0.93},{1.23,0.82},{1.22,1.01},{0.76,1.06},{1.03,0.98},{1.25,0.89},{0.72,0.67},{1.15,0.9},{0.75,0.58},{0.6,1.21},{0.89,0.65},{1.09,0.98},{1.45,0.88},{0.82,1.28},{0.83,1.44},{1.05,1.17},{1.26,0.6},{0.68,0.82},{1.03,0.93},{0.91,0.94},{0.5,1.05},{1.01,1.23},{0.66,0.94},{1.34,0.65},{1.33,1.12},{0.87,0.91},{1.01,1.05},{0.53,1.13},{0.62,0.86},{0.76,0.8},{1.18,0.86},{1.09,0.65},{0.69,0.76},{1.07,0.83},{1.03,1.04},{1.35,0.76},{1.1,1},{1.48,1.13},{0.98,0.94},{1.22,0.64},{0.99,1.28},{1.06,1.38},{0.97,1.07},{0.97,1},{0.93,1.42},{0.65,0.76},{1.21,1.37},{0.89,0.93},{0.99,1.02},{0.9,0.75},{1.1,0.76},{0.87,1.12},{1.3,1.1},{1.03,1.28},{1.05,1.16},{1.37,1.17},{0.64,1.02},{1.29,1.1},{1.06,0.9},{0.89,1},{0.78,0.98},{1.24,1.22},{0.76,1.39},{1.01,1.03},{1.34,0.95},{0.98,0.73},{0.84,1.33},{0.74,1.3},{1.26,1.38},{0.88,1.21},{1.25,1.03},{1,1.02},{1,1},{1.11,0.89},{0.97,0.5},{1.12,0.87},{0.99,0.99},{0.83,0.85},{1.12,1.31},{1.24,1.31},{0.59,0.88},{0.84,1.45},{1.35,0.88},{0.98,1.1},{0.77,0.56},{1.03,1.03},{1.08,0.96},{1.04,1.44},{1.19,0.88},{0.79,1.38},{1.14,1.15},{2.03,3.05},{2.15,2.65},{1.91,2.98},{2.11,2.72},{1.8,3.4},{1.85,3.07},{2.29,2.83},{2.26,3.25},{1.98,2.98},{2.23,2.89},{2.25,3.29},{1.96,2.88},{1.87,2.67},{1.67,3.3},{1.87,2.85},{2.45,2.81},{2.01,2.79},{2.13,3.36},{2.03,2.56},{1.79,3.1},{2.13,3.11},{1.75,3.36},{1.95,2.96},{2.35,3.2},{1.77,3.3},{2.27,2.88},{1.98,2.77},{1.85,2.85},{2.01,3.5},{2.13,2.65},{1.68,3.32},{1.84,2.73},{1.64,3.15},{2.18,3.22},{2.34,2.8},{1.95,3.01},{1.77,3.06},{2.01,3.02},{2.02,3.01},{2.06,3.27},{1.62,3.22},{1.9,2.78},{1.98,3.12},{2.04,2.89},{2.09,2.97},{2.03,3},{1.82,3},{2.35,2.98},{1.95,3.38},{2.08,2.77},{2.28,3.02},{2.06,3.01},{1.97,3.4},{1.86,3.14},{2.12,3.12},{2.2,2.86},{2,3},{2.17,3.09},{1.93,2.92},{2.09,3.35},{2,2.96},{2.22,3.02},{1.75,3.35},{2.14,3.07},{1.87,3.13},{2.03,3.12},{2.22,2.95},{2.16,3.12},{1.68,2.91},{2.24,3.09},{2.13,2.77},{1.78,2.92},{1.99,2.99},{2.09,2.95},{1.91,2.71},{2,3},{1.91,3.19},{2.01,3.02},{1.66,3.2},{1.99,3.01},{2.16,2.78},{1.75,3.28},{1.98,2.62},{2,3.01},{2.01,3.04},{1.71,2.73},{1.55,2.97},{1.91,2.83},{2.04,3.05},{2.28,2.98},{2.14,2.67},{2.1,3.32},{2.05,2.93},{2.01,3.02},{2.33,2.99},{1.97,3.02},{2.09,2.98},{2.31,2.98},{2.03,2.82},{2.33,3},{1.97,2.67},{1.87,2.81},{2.26,3.23},{2.2,3.13},{2.36,3.15},{1.85,2.96},{1.98,2.92},{2.32,2.95},{1.66,2.78},{2.03,3.21},{2.08,3.27},{2.03,2.54},{1.57,3.14},{1.87,2.95},{1.79,2.58},{2.04,2.93},{1.93,2.95},{1.91,3.08},{2.3,2.63},{2.13,2.55},{1.85,2.98},{2.1,2.69},{2.09,3.04},{1.92,2.55},{1.89,3.15},{2,3.05},{1.75,3.02},{2.11,3.24},{1.7,2.86},{2.33,2.75},{1.99,2.99},{2.17,3.04},{2.06,2.92},{2,3.24},{2.01,3.02},{1.92,2.85},{2.13,3.29},{1.99,2.74},{1.84,2.6},{2.37,3.19},{2.1,3.45},{2.16,3.09},{1.66,2.95},{2.14,2.95},{2.06,3.13},{1.97,3.05},{2.22,2.84},{1.92,3.15},{1.77,2.98},{2.05,2.8},{2.17,3.1},{1.96,2.93},{2.04,2.9},{2.11,3.39},{2.17,2.75},{1.79,3.33},{1.77,3.18},{1.77,2.85},{2,3.01},{2.14,2.69},{2.34,3.26},{1.73,2.83},
		{1.74,3.29},{1.9,2.84},{2.04,3.04},{2.04,2.94},{1.78,3.18},{2.25,3.24},{2,2.93},{1.86,3.03},{1.69,3.17},{2.27,3.39},{1.83,3.33},{2.01,3.04},{1.79,2.98},{2.1,3.09},{1.79,3.21},{2.02,2.75},{1.91,3.21},{1.53,3.12},{2.01,2.99},{1.76,2.61},{2.33,3.26},{1.78,3.06},{2.4,2.78},{2.08,3.33},{2,3},{1.98,3.37},{2,2.96},{1.64,2.81},{2.02,2.97},{2.02,3.01},{2.25,2.88},{2.19,2.85},{2.04,3.47},{2.23,2.59},{1.99,2.68},{1.69,3.39},{1.93,3.09},{1.93,3.03},{1.9,2.92},{2.06,2.98},{1.88,3.21},{1.99,2.96},{1.85,2.86},{1.81,3.3},{2.33,3.26},{2,2.92},{2.26,3.33},{1.77,2.77},{1.85,3.33},{1.69,2.95},{2.05,3.13},{1.91,3.11},{2.05,2.93},{1.93,3.2},{2.1,2.87},{2.26,3.25},{1.97,3.25},{2.16,3.03},{2.38,2.84},{2.05,2.81},{1.6,3.15},{2.3,2.8},{1.63,2.78},{1.61,3.09},{2.3,3.11},{1.99,2.99},{1.84,2.8},{2.09,2.91},{2.02,3.2},{2.18,3.33},{1.82,3.04},{1.98,3.01},{2.04,3.24},{2,2.93},{2.01,3.01},{2.01,3.25},{2.24,2.75},{1.74,3.28},{2.12,2.91},{1.99,3.02},{1.99,2.99},{2.12,3.02},{1.54,2.99},{1.87,3.09},{1.91,2.9},{1.67,3.09},{1.88,3.16},{1.95,2.52},{1.73,3.12},{2.35,2.84},{2.14,3.09},{1.83,3.46},{1.86,3.07},{1.8,2.66},{2.02,3},{2.04,2.76},{2.02,2.56},{1.95,3.06},{1.78,3.2},{1.71,2.81},{1.74,2.62},{2.27,3.35},{1.71,3.02},{1.8,2.99},{1.98,3.18},{2.04,2.99},{1.98,3.29},{1.84,2.76},{2.06,3.06},{1.65,2.92},{1.81,3.14},{1.95,2.97},{2.21,2.75},{1.86,3.28},{2.47,3.08},{2.18,2.62},{2.29,2.73},{1.91,3.04},{2,2.98},{2,3},{2.33,2.73},{2.02,2.96},{2.37,2.99},{2.03,2.78},{2.24,2.94},{2.1,3.08},{1.91,3.14},{2.38,3.32},{1.84,2.9},{1.89,3.11},{2.19,2.72},{1.57,3.07},{2.11,3.09},{2,3},{1.86,2.87},{1.76,3.04},{1.69,2.95},{1.91,3.09},{2.07,2.75},{1.63,3.17},{2.04,3.27},{2.31,2.7},{2.06,3.22},{1.66,3.24},{2.02,3.13},{2.2,3.38},{2.23,3.22},{2.04,3.11},{2.21,2.96},{1.58,2.75},{1.89,2.65},{2.18,3.22},{1.98,3.02},{2.07,3.48},{2.13,2.71},{1.99,2.98},{1.91,3.08},{2.09,2.89},{2,3},{2.13,2.84},{2.22,3.16},{1.68,2.9},{2.02,2.79},{1.98,3.32},{2.16,2.93},{2.09,3.08},{1.9,3.06},{1.81,2.97},{1.52,3.11},{1.92,3.11},{2.18,2.94},{1.75,3.15},{1.63,3.32},{2.19,3.2},{1.98,3.12},{2.14,3.48},{1.82,2.98},{2.17,3.19},{1.73,3.27},{1.87,2.93},{2.25,2.72},{1.74,3.12},{1.82,3.14},{2.3,3.31},{2.27,3.11},{2.23,3.31},{1.83,3.25},{2,3.05},{1.94,3.08},{1.95,3.31},{2,2.99},{1.99,2.94},{2.31,3.03},{2.22,3.17},{2.43,2.79},{2.39,2.73},{1.95,2.9},{1.96,3.38},{2.02,2.67},{2,2.82},{2.02,3.07},{1.74,3.34},{2.09,3.02},{2.02,3},{1.62,3.18},{1.79,3.3},{1.62,3.17},{1.86,3.09},{1.93,2.87},{2.37,3.12},{2.17,2.98},{2.15,2.89},{2.19,3.35},{2.29,3.07},{1.72,2.78},{1.84,2.73},{2.14,3.07},{1.96,2.5},{1.68,3.24},{2.38,2.69},{1.85,3.26},{2.09,3.08},{1.58,3.18},{1.91,2.68},{2.09,3.05},{1.92,3.16},{2.16,2.62},{2.15,2.99},{2.15,3.13},{2.38,2.97},{2.31,3.06},{2.06,3.36},{2,3},{2.06,3.39},{1.73,2.6},{2.06,2.88},{2.24,2.84},{1.61,3.08},{1.97,2.79},{1.69,3.33},{1.86,2.84},{2.27,3.08},{1.87,2.87},{1.97,3.03},{1.95,3.28},{1.81,3.03},{1.78,2.81},{1.95,3.19},{2,3.33},{2.23,2.9},{1.87,2.96},{2.26,3.02},{2.29,3.04},
		{2.12,2.93},{1.97,2.89},{2.11,2.87},{1.73,2.99},{2.11,3.03},{2.07,3.22},{2,3.19},{1.82,2.61},{2.04,3.02},{2.14,3.08},{2.24,2.74},{1.76,2.97},{2.09,2.91},{1.89,3.14},{2.26,3.15},{2.23,3.11},{1.79,2.99},{1.87,3.02},{2.07,3.04},{2.27,2.8},{1.82,2.57},{1.87,2.98},{2.5,3.03},{1.96,3.04},{1.88,3.2},{2.05,2.79},{2.06,3.06},{2.01,3.09},{1.92,3.23},{1.99,3.49},{1.94,3.03},{1.9,3.01},{2.15,3.11},{2.42,2.97},{1.97,3.22},{1.75,3.03},{2.05,3.37},{1.95,3.38},{2.02,3.28},{1.78,2.98},{1.89,3.02},{2,2.99},{1.87,2.84},{1.95,3.22},{1.95,2.96},{2.22,2.78},{1.88,3.12},{1.96,3.29},{2.08,3.01},{2.06,3.29},{1.92,2.6},{2.26,3.13},{1.9,2.83},{1.8,3.09},{2.01,3.01},{1.86,3.01},{2.07,3.18},{2.16,3.25},{1.75,3.24},{1.61,2.82},{1.87,3.27},{2.19,3.26},{1.6,2.76},{2.1,2.58},{1.95,3.31},{2.17,3.09},{1.78,3.04},{2.42,3.19},{2.3,3.39},{2.07,2.87},{1.84,2.77},{2,2.9},{2.38,3},{1.75,2.71},{1.74,3.24},{1.95,3.07},{2.1,2.93},{2.11,2.87},{1.95,3.03},{2.08,3.21},{1.76,3.12},{1.99,2.82},{2.13,2.82},{1.99,2.96},{2,3.09},{3.05,1.08},{3.24,0.65},{3.23,0.95},{2.78,0.88},{2.99,1.22},{3.33,1.25},{3,1.02},{3.33,1.3},{2.99,1},{3.02,0.97},{2.89,1.09},{2.86,0.98},{3,1},{2.92,1.39},{2.97,1.03},{3.21,0.6},{2.87,1.01},{3.28,1.14},{3.13,1.02},{3.32,0.78},{2.97,1.06},{3.2,0.92},{3.29,0.66},{3.01,0.91},{3,1.03},{2.79,1.09},{3.09,1.02},{3,1.34},{2.98,1.07},{3.22,0.65},{3.12,1.03},{3.4,1.26},{3.41,1.29},{2.86,0.6},{2.56,1.15},{2.73,0.9},{3.11,0.78},{3.13,1.18},{3.06,0.98},{2.9,1.1},{3.01,1.03},{2.93,0.55},{3.13,1.02},{3.07,0.93},{2.88,1.03},{2.93,0.92},{3.26,0.69},{3.35,0.66},{3,0.7},{3.03,1.01},{3.01,1.27},{3.38,0.91},{2.88,1.15},{2.79,0.71},{3.23,1.05},{3.02,0.84},{3.14,1.24},{3.24,1.21},{3.26,0.98},{2.93,1.31},{2.84,1.45},{2.98,1.03},{3.01,0.99},{2.97,1.21},{2.58,0.78},{2.93,1},{2.94,0.51},{3.09,0.62},{3.15,0.89},{3.19,1.12},{3.43,0.95},{3,1.04},{3.25,1.25},{3,0.98},{2.77,1.22},{3.12,1.35},{2.95,0.99},{2.98,1.03},{3,0.82},{3.4,1.29},{2.69,1.08},{2.8,1.34},{3.13,1.19},{3.33,1.28},{3.33,0.79},{2.87,0.85},{3.08,0.97},{3.26,0.98},{2.95,1.16},{3.26,0.92},{2.86,1.29},{3.23,1.36},{3.19,1.44},{3.04,1.48},{3.04,1.15},{2.99,0.72},{3.11,0.66},{3.4,1.05},{2.92,0.88},{3.22,1.19},{3.37,1.06},{2.69,1.2},{2.98,0.98},{2.83,1.12},{3.01,0.99},{3.06,0.99},{3.34,0.84},{2.93,1.02},{2.96,0.88},{2.97,1.27},{2.85,0.88},{2.85,0.75},{2.99,1.04},{2.92,1.39},{3.22,1.11},{3.44,1.14},{3.37,1.11},{2.88,1.02},{2.99,1.1},{3.25,1.14},
		{3.28,0.8},{3.12,1.26},{3.17,1.46},{3.22,1.36},{3.11,0.55},{2.74,0.66},{3,0.98},{3.35,0.79},{3.39,1.08},{3.04,1.23},{2.97,0.64},{2.98,0.8},{3.01,1.02},{3.21,1.11},{3.46,1.07},{3.11,0.99},{3.1,1.08},{2.96,0.62},{3.06,0.61},{2.57,1.22},{2.96,1.27},{2.91,0.67},{2.7,0.9},{3.24,1.21},{2.59,0.96},{2.66,1.04},{3.24,0.65},{3.12,0.78},{3.09,1.28},{3.15,0.86},{2.91,0.99},{3.03,1.17},{3.09,0.59},{2.66,0.8},{2.85,1.19},{3.11,1.02},{2.86,0.74},{3,1.12},{3.01,0.99},{2.67,0.77},{3.1,0.82},{3.27,1.16},{2.74,1.22},{2.91,1.01},{3.18,1.26},{3.14,1.48},{2.8,0.89},{3.01,0.92},{2.99,0.98},{3.23,0.83},{3.14,1.03},{2.93,1.04},{3.32,1.38},{2.61,0.93},{3.16,0.7},{2.94,0.99},{3.36,0.69},{3.16,0.99},{3.01,1.07},{3.07,0.74},{2.98,1.02},{2.63,0.93},{2.56,0.9},{2.83,0.79},{2.9,0.91},{2.82,1.06},{2.92,1.03},{3.1,0.91},{2.94,0.99},{2.56,0.89},{3.45,1.12},{3.32,1.14},{2.7,1.04},{2.85,1.24},{2.86,1.13},{3.06,1.29},{2.91,1.12},{2.8,0.84},{2.78,1.27},{2.89,1.28},{3.01,0.95},{3.16,1.14},{2.84,1},{2.98,0.62},{3.29,1.05},{3.28,0.78},{2.86,0.97},{2.86,1.03},{3.31,1.14},{2.73,0.59},{2.71,1.13},{3.05,0.66},{3.01,0.79},{3.16,0.88},{2.97,0.97},{3,1},{2.9,0.92},{2.73,0.96},{2.79,1.01},{3.04,0.81},{3.31,1.05},{3.24,1.39},{3.17,0.98},{3.42,1.03},{2.64,1.29},{2.88,0.67},{3.18,0.59},{2.97,0.97},{2.83,1.27},{2.97,1.13},{3.08,1.42},{2.85,0.95},{2.97,0.76},{2.61,0.8},{2.67,0.71},{2.85,1.02},{3.18,0.8},{2.98,1.3},{3.11,0.99},{2.97,0.96},{3.03,1.02},{2.91,0.76},{2.96,1.08},{3.11,0.97},{2.64,0.85},{3.01,1.07},{2.6,1.07},{3.1,1.22},{2.76,0.82},{3.01,0.99},{3,1},{2.62,0.72},{3.09,1.25},{3.38,0.84},{3.13,0.8},{3.27,1.36},{2.98,0.82},{3.41,0.91},{3.2,0.99},{2.83,1.33},{3.04,1.05},{3.17,0.99},{3.02,1.01},{2.88,1.02},{3.46,0.86},{3.2,0.74},{2.96,0.99},{3.31,1.13},{2.75,1.06},{2.7,1.14},{3.28,1.09},{3,1},{2.93,0.85},{3.25,0.79},{3.24,1.02},{2.77,0.99},{3,1},{2.72,1.07},{2.72,1.07},{3.03,1.01},{2.94,1.14},{2.89,1.09},{3.03,0.99},{3.34,0.9},{2.85,1.11},{2.89,1.03},{3.14,0.73},{2.97,0.94},{3.36,1.3},{3.11,1.09},{3.11,1.29},{3.02,1.39},{2.89,1.19},{3.08,0.71},{3,1},{3.39,0.86},{3.15,0.93},{3.09,0.53},{3,1.01},{3.34,0.79},{2.67,0.71},{3.26,0.73},{3.12,0.92},{2.94,0.97},{3.27,0.82},{3.01,1.02},{2.82,1.2},{2.87,1.43},{2.99,0.98},{3.03,1},{2.57,0.81},{3.01,1.13},{2.95,1.03},{3.2,0.94},{2.65,1.27},{3,1.02},{3.06,0.99},{2.81,0.94},{2.94,1.06},{3.05,1.49},{2.72,0.79},{3.16,0.71},{2.62,1.22},{3.39,0.87},{3.38,1.03},
		{3.1,1.2},{2.94,0.91},{2.98,0.98},{2.73,1.08},{2.82,0.82},{2.53,0.98},{2.92,0.81},{2.68,1.3},{3.01,0.97},{2.89,1.32},{2.95,1.27},{2.91,0.91},{2.88,1.35},{3.29,0.92},{3.04,1.01},{2.9,0.98},{3.11,1.4},{3,1},{3,1.01},{3.07,1.1},{3,1},{2.77,0.89},{3.21,0.82},{3.04,1.02},{2.75,1.28},{2.95,0.53},{3.08,1.17},{3.44,0.87},{2.93,0.54},{3.27,1.1},{2.64,0.89},{2.84,0.96},{3.44,1.2},{3,0.7},{3.34,0.94},{3.19,0.99},{3.03,0.93},{2.92,1.07},{3,1.12},{3.04,1.12},{3.03,0.93},{3,0.97},{2.98,0.97},{2.89,1.03},{2.99,0.97},{3.05,0.76},{3.13,1.01},{3.22,0.91},{3.3,0.63},{2.8,1.09},{3.25,1.08},{2.78,1.43},{3.23,0.64},{3.01,1.07},{3.32,1.11},{2.68,0.84},{2.88,0.95},{3,1.17},{2.83,1.43},{3.35,1.21},{3,0.96},{3.05,1.02},{3.12,1.46},{2.83,0.88},{3.31,0.89},{3.08,1.03},{3.18,1.33},{2.97,1.28},{2.9,0.92},{3.41,0.75},{3.1,1.07},{2.52,0.93},{3.05,0.73},{2.99,1.31},{2.77,0.96},{2.79,1.05},{2.94,1.03},{3.23,0.64},{2.9,1.07},{2.99,1.01},{3.23,1.15},{2.97,1.36},{2.96,1.06},{2.96,0.99},{2.92,0.63},{2.9,0.86},{2.65,1.18},{2.62,1.3},{2.52,1.11},{2.99,1.02},{2.87,1.02},{2.77,0.97},{2.65,1.11},{3.14,0.87},{3.21,1.25},{2.99,1.34},{3.24,1.02},{3.03,0.99},{2.99,0.99},{2.97,1.07},{3.34,0.94},{2.96,1.04},{2.91,1.02},{3.34,1.28},{2.99,1.09},{2.84,0.7},{2.76,1.04},{2.99,1},{3,0.99},{2.68,0.96},{3.04,0.99},{2.64,1.07},{3.09,0.89},{3.13,0.83},{3.02,0.68},{2.87,1.29},{2.95,0.89},{2.98,0.94},{3.13,0.97},{2.9,1.05},{3.04,1.33},{3.05,1.24},{3.33,0.8},{2.75,1.21},{2.9,0.97},{3.08,0.87},{3.06,1.08},{2.95,0.55},{2.68,1.09},{3.03,1.09},{3,1},{2.91,1.15},{2.92,0.59},{3.03,1.47},{3.12,0.7},{3.03,1.17},{3.22,1.27},{3.01,1.37},{2.89,0.89},{2.94,1.05},{2.82,0.83},{3.25,0.95},{3,0.98},{3.12,0.87},{3.19,1.17},{3.45,0.84},{3.17,0.99},{3.05,1.01},{3.19,1.13},{3.15,1.28},{3.24,1.25},{2.9,0.97},{3.11,1.07},{3,1},{2.66,0.87},{2.67,1.19},{2.98,1.09},{3.04,1.06},{3,1.25},{2.96,1.04},{2.66,1.3},{2.85,1.12},{3.3,1.2},{3.19,1.08},{2.5,1.03},{2.65,0.88},{2.92,1.41},{3.28,0.67},{2.9,1.21},{3.04,1.16},{3.25,0.73},{2.79,0.76},{2.91,1.02},{2.61,0.73},{3.46,1.04},{1.47,3.2},{1.93,0.74},{0.42,2.04},{1.21,2.65},{0.77,0.85},{0.72,0.25},{0.37,1.02},{1.01,1.9},{3.98,1.64},{1.33,1.02},{3.15,2.84},{0.3,2.07},{2.55,3.89},{1.45,3.27},{1.59,3.42},{1.97,3.87},{1.75,2.29},{3.93,1.3},{1.56,3.43},{0.32,0.97},{3.45,1.96},{3.39,1.15},{1.23,2.18},{1.46,1.82},{0.31,2.06},{2.77,3.88},{3,3.24},{2.57,0.92},{0.19,1.27},{3.59,1.09}};

c={{{2,2,2.2}},{{1,1,1.5},{2.1,3.2,1.5},{3.2,1.2,1.5}},{{0.35,2.5,1},{1,1,1},{2.1,3.2,1},{3.2,1.2,1},{3.7,3,1}},{{1.04,2.2,0.75},{0.8,0.9,0.75},{2.1,3.2,0.75},{3.23,1.1,0.75},{3.7,3,0.75},{2,0.8,0.75},{2.8,4.2,0.75},{4,1.9,0.75}},{{0.5,2.2,0.5},{1.4,2.2,0.5},{0.67,1.2,0.5},{1.1,.5,.5},{2,3,0.5},{1.5,3.7,.5},{3,1,0.5},{3.3,3,0.5},{1.6,1.1,0.5},{2.8,4.2,0.5},{3.7,1.55,0.5}},{{0.4,2.2,0.25},{1.25,1.95,0.25},{0.4,1.2,0.25},{.8,.4,.25},{1.9,2.45,0.25},{1.6,3.2,.25},{3,.7,0.25},{3.15,3.05,0.25},{1.3,1.1,0.25},{2.6,3.9,0.25},{3.8,1.5,0.25},{2,.9,.25},{2,3.8,.25},{1.45,2.65,.25},{1.9,2.9,.25},{2.3,2.65,.25},{2.3,3.15,.25},{2,3.35,.25},{0.85,1.3,.25},{1.15,1.5,.25},{.5,.8,.25},{.85,.9,.25},{1.15,.5,.25},{1.3,.8,.25},{3.5,1.8,.25},{3.3,.8,.25},{3.4,1.2,.25},{3,1.1,.25},{3.05,1.5,.25},{2.6,1.4,.25},{2.6,.8,.25},{2.6,1,.25}}};

r={1.05,1.24,1.30,1.17,1.06,0.97,1.11,1.30,1.14,0.88,1.04,1.23,0.98,0.88,1.18,1.04,0.81,0.9,1.05,0.79,0.74,1.012,0.86,0.621,0.69,0.91,0.81,0.62,0.508,0.53,0.72,0.87,0.83,0.82,0.70,0.72,0.712,0.69,0.693,0.70,0.75,0.75,0.73,0.74,0.76,0.733,0.717,0.747,0.72,0.75,0.72,0.719,0.74,0.735,0.71,0.75,0.74,0.73,0.74,0.74,0.74,0.72,0.742,0.746,0.72,0.749,0.80,0.86,0.94,0.954,0.983,0.958,0.95,0.98,0.98,1.11,1.21,1.32,1.19,1.06,0.94,1.12,1.311,1.15,0.90,1.03,1.25,0.99,0.89,1.16,1.025,0.83,0.95,1.10,0.79,0.68,1.01,0.87,0.64,0.67};

End[]
Protect[GenerateRandomPatternNative]
Protect[GenerateRandomPatternInt]
Protect[GenerateRandomPatternFloat]
Protect[ComputePercentVariation]
Protect[GenerateRandomVariantInt]
Protect[GenerateRandomVariantNative]
Protect[GenerateRandomVariantFloat]

Protect[GetVersion]
Protect[OpenNano]
Protect[CloseNano]
Protect[NanoList]
Protect[ExistsQ]
Protect[LearningQ]
Protect[SetLearningStatus]
Protect[SaveNano]

Protect[GetConfig]
Protect[ConfigureNano]
Protect[AutotuneConfig]

Protect[RunNano]
Protect[LoadData]
Protect[RunStreamingData]
Protect[GetNanoStatus]
Protect[DecodePM]
Protect[GetNanoResults]
Protect[GetBufferStatus]

EndPackage[]