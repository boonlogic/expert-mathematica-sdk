(* Wolfram Language Package *)

BeginPackage["NanoREST`"]

GetBufferStatus::usage="GetBufferStatus[NanoHandle] returns information on the buffer"

GetNanoStatus::usage="GetNanoStatus[NanoHandle] returns statistics on the clusters"

GetNanoResults::usage="GetNanoResults[NanoHandle] returns the clustering results"


DecodePM::usage="DecodePM[NanoHandle] returns the pattern memory (base64) or any of the results specified"

GetThreshold::usage="GetThreshold[NanoHandle, accuracy] returns a value from 0 to 1000 for the anomaly index threshold"


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

Begin["`Private`"] (* Begin Private Context *) 


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

SortStatusResults[ResultString_] := Module[{new = {}, vals = {"PCA","patternMemory","clusterGrowth","clusterSizes","anomalyIndexes","frequencyIndexes","distanceIndexes","totalInferences","averageInferenceTime","numClusters","disArray"}},
	Do[new=Join[new,Position[ResultString,ToString[i]]],{i,vals}];
	new=DeleteDuplicates[ResultString[[Flatten[new]]]];
	new=StringDelete[ToString[#],{"NanoREST`"}]&/@new;
	Return[new]
]

Options[GetNanoStatus]={Results->All};
GetNanoStatus[NanoHandle_,OptionsPattern[]]:=Module[{req,url,RetVal,ResultString,results},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	results=ToString/@Flatten[{OptionValue[Results]}];
	results=StringDelete[#, {" ", "{", "}","NanoREST`","Private`"}]&/@results;
	If[SubsetQ[{"All","PCA","patternMemory","clusterGrowth","clusterSizes","anomalyIndexes","distanceIndexes","frequencyIndexes","totalInferences","averageInferenceTime","numClusters","disArray"},results]==False,
		Message[InvalidOption::option,ToString[OptionValue[Results]],ToString[Results]];
		Return[]
	];
	If[MemberQ[Flatten[{OptionValue[Results]}],All],
		ResultString=ToString[{PCA,clusterGrowth,clusterSizes,anomalyIndexes,frequencyIndexes,distanceIndexes,totalInferences,numClusters}], (* list out all result options *)
		ResultString=ToString[SortStatusResults[results]]; (* not None or All so must be a subset of result options *)
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

SortResultsResults[ResultString_]:=Module[{new={},vals={"ID","SI","RI","FI","DI"}},
	Do[new=Join[new,Position[ResultString,ToString[i]]],{i,vals}];
	Return[ToString[DeleteDuplicates[ResultString[[Flatten[new]]]]]]
]

Options[GetNanoResults]={Results->All};
GetNanoResults[NanoHandle_,OptionsPattern[]]:=Module[{req,url,RetVal,ResultString},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[SubsetQ[{"All","ID","SI","RI","FI","DI","MD"},ToString/@Flatten[{OptionValue[Results]}]]==False,
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

GetThreshold[NanoHandle_,accuracy_:0.999]:=Module[{status,scale,p,threshold},
	status=GetNanoStatus[NanoHandle];
	scale=Total[Table[Max[status["clusterSizes"]]^(1-x)/status["totalInferences"],{x,0.001,1,.001}]];
	p = Table[{pFit,Total[(
		Table[scale * pFit * (1-pFit)^(x-1),{x, status["anomalyIndexes"]}] -
      	status["clusterSizes"]/status["totalInferences"]*1.)^2]},
      	{p, 0.001, 0.02, 0.0001}]
	p = First[First[Sort[p, #1[[2]] < #2[[2]] &]]]
	threshold=Ceiling[Log[1-accuracy]/Log[1-p]];
	Return[Clip[threshold,{0,1000}]]
]

Options[DecodePM]={Results->"BinaryPM",RowSort->False,Scaled->True};
DecodePM[NanoHandle_,OptionsPattern[]]:=
	Module[{results,config,strm,ratio,pad,pca,PM,ord,PMDecimal,numberOfEachSample,sampleSortedPM,image,temp,sourceVec,cmyk,x,y,z,xClust,yClust,min,max,sortOrder,returnList={},samples,featSig},
		If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
		results=Flatten[{OptionValue[Results]}];
		results=StringDelete[#, {" ", "{", "}","NanoREST`","Private`"}]&/@results;
		If[SubsetQ[{"CMYK","SourceVector","BinaryPM","Features",All},results]==False,
			Message[InvalidOption::option,ToString[OptionValue[Results]],ToString[Results]];
			Return[]
		];
		If[MemberQ[{True,False},OptionValue[RowSort]]==False,
			Message[InvalidOption::option,ToString[OptionValue[RowSort]],ToString[RowSort]];
			Return[]
		];
		
		If[MemberQ[results,All],results={"CMYK","SourceVector","BinaryPM","Features"}];
		
		config=GetNanoStatus[NanoHandle,Results->{"patternMemory",PCA,"disArray"}];
		If[config===Null,Message[NanoWarning::message,"No results to be returned"];Return[]]; (* error *)
		{PM,pca,samples}=Values[KeyTake[config,{"patternMemory","PCA","disArray"}]];
		PMDecimal=ImportString[#,{"Base64","Binary"}]&/@PM;
		PM=Partition[Flatten[Reverse[IntegerDigits[#,2,8]]&/@Flatten[PMDecimal]],Length[PMDecimal[[1]]]*8];
		
		config=GetConfig[NanoHandle];
		If[config===Null,Message[NanoWarning::message,"No results to be returned"];Return[]]; (* error *)
		strm=config["streamingWindowSize"];
		{min,max}=Transpose[Values[KeyTake[#,{"minVal","maxVal"}]]&/@(config["features"])];
		pca=Drop[pca,1];
		min=Flatten[ConstantArray[min,strm]];
		max=Flatten[ConstantArray[max,strm]];
		     
		If[MemberQ[results,"BinaryPM"],
			returnList={"BinaryPM"->PM};
		];
		
		(* if only returning binary pm, return before calculating the rest *)
		If[results==={"BinaryPM"},
			Return[Association[returnList]]
		];
		
		If[OptionValue[RowSort],
			{x,y,z}=Flatten[Quiet[Table[First[Position[pca,_?(#[[i]]==Max[Transpose[pca][[i]]]&),1]],{i,1,3}]]];
			xClust=Select[Complement[Range[Length[pca]],{x,y,z}],pca[[#,1]]>=pca[[#,2]]&];
			yClust=Select[Complement[Range[Length[pca]],{x,y,z}],pca[[#,1]]<pca[[#,2]]&];
			yClust=yClust[[Reverse[Ordering[pca[[yClust,3]]]]]];
			xClust=xClust[[Ordering[pca[[xClust,3]]]]];
			sortOrder=Flatten[{x,xClust,z,yClust,y}];
		];
		
		numberOfEachSample=BinCounts[samples,{0,Length[min]}];
		ord=TakeList[Ordering[samples],numberOfEachSample];
		sampleSortedPM=PM[[All,#]]&/@ord;
		image=Transpose[Table[If[numberOfEachSample[[i]]!=0,Count[#,0]&/@sampleSortedPM[[i]]/numberOfEachSample[[i]]*1.,ConstantArray[0,Length[sampleSortedPM[[i]]]]],{i,1,Length[numberOfEachSample]}]];
		
		Print[Dimensions[numberOfEachSample]];
		Print[Dimensions[ord]];
		Print[Dimensions[sampleSortedPM]];
		Print[Dimensions[image]];
		Print[strm];
		Print[min];
		Print[max];
		
		If[MemberQ[results,"SourceVector"],
			If[OptionValue[Scaled],
				sourceVec=(#*(max-min)+min)&/@image;,
				sourceVec=image;
			];
			returnList=Join[returnList,{"SourceVector"->sourceVec}];
		];
		
		If[MemberQ[results,"CMYK"],
			cmyk=Table[Join[pca[[#]],{image[[#,i]]}],{i,1,Length[image[[1]]]}]&/@Range[Length[image]];
			ratio=Clip[Round[2\[Pi]/(.1*Length[pca])],{1,\[Infinity]}];
			pad=Clip[Round[#[[2]]/ratio,#[[1]]]/#[[1]],{1,\[Infinity]}]&@(Dimensions@cmyk);
			cmyk=Flatten[ConstantArray[#,pad]&/@cmyk,1];
			returnList=Join[returnList,{"CMYK"->cmyk[[If[OptionValue[RowSort],sortOrder*pad,All]]]}];
		];
		If[MemberQ[results,"Features"],
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


End[] (* End Private Context *)

Protect[GetNanoStatus]
Protect[DecodePM]
Protect[GetThreshold]
Protect[GetNanoResults]
Protect[GetBufferStatus]

EndPackage[]