(* Wolfram Language Package *)

BeginPackage["NanoREST`"]
(* Exported symbols added here with SymbolName::usage *)  
LoadData::usage="LoadData[NanoHandle,data] posts the data to be clustered"

RunNano::usage="RunNano[NanoHandle] clusters the data"

RunStreamingData::usage="RunStreamingData[NanoHandle, data] pushes data through the nano and autotunes in the process"

AppendData::usage="option for combining new data to previous data not yet clustered"
GZip::usage="option for loading data in a zip format"


Begin["`Private`"] (* Begin Private Context *) 

(******** CLUSTER ********)
(* Uploads the data to be clustered and returns any results specified *)
Options[LoadData]={AppendData->False,GZip->False};
LoadData[NanoHandle_,Data_,OptionsPattern[]]:=Module[{bytes,req,url,RetVal,NumericFormat,tempFile},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[Data===$Failed,Message[NanoWarning::message,"Invalid data"];Return[]];
	
	If[MemberQ[{True,False},OptionValue[AppendData]]==False,
		Message[InvalidOption::option,ToString[OptionValue[AppendData]],ToString[AppendData]];
		Return[]
	];
	
	If[MemberQ[{True,False},OptionValue[GZip]]==False,
		Message[InvalidOption::option,ToString[OptionValue[GZip]],ToString[GZip]];
		Return[]
	];
	
	NumericFormat=GetConfig[NanoHandle];
	If[NumericFormat=!=Null,NumericFormat=NumericFormat["numericFormat"],
		Return[]];
	tempFile=CreateFile[];
	bytes=ExportByteArray[Data,Which[NumericFormat=="float32","Real32",NumericFormat=="uint16","UnsignedInteger16",NumericFormat=="int16","Integer32",True,NumericFormat]];
	BinaryWrite[tempFile,bytes];
	Close[tempFile];
	
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
	"Body"->{"data"->File[tempFile]}
	|>];
	RetVal= URLRead[req,{"Status","Body"}];
	DeleteFile[tempFile];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
	];
]

(* Uploads the data to be clustered for streaming applications *)
Options[RunStreamingData]={Results->ID,GZip->False};
RunStreamingData[NanoHandle_,Data_,OptionsPattern[]]:=Module[{bytes,ResultString,req,url,RetVal,NumericFormat,tempFile},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	
	If[SubsetQ[{"None","All","ID","SI","RI","FI","DI","MD"},ToString/@Flatten[{OptionValue[Results]}]]==False,
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
	tempFile=CreateFile[];
	bytes=ExportByteArray[Data,Which[NumericFormat=="float32","Real32",NumericFormat=="uint16","UnsignedInteger16",NumericFormat=="int16","Integer32",True,NumericFormat]];
	BinaryWrite[tempFile,bytes];
	Close[tempFile];
	
	url=NanoHandle["url"]<>"nanoRunStreaming/"<>NanoHandle["instance"]
	<>"?fileType=raw"
	<>"&gzip="<>ToLowerCase[ToString[OptionValue[GZip]]]
	<>"&results="<>StringDelete[ResultString, {" ", "{", "}","NanoREST`","Private`"}]
	<>"&api-tenant="<>NanoHandle["api-tenant"];

	req = HTTPRequest[url, 
	<|
	"Method" ->"POST",
	"Headers"->{"Content-Type"->"multipart/form-data","x-token"->NanoHandle["api-key"],"type"->"text/csv"},
	"Body"->{"data"->File[tempFile]}
	|>];
	RetVal= URLRead[req,{"Status","Body"}];
	DeleteFile[tempFile];
	If[RetVal[[1]]!=200 && RetVal[[1]]!=201,
		Message[NanoError::return,ToString[RetVal[[1]]],RetVal[[2]]];
	];
	Return[ImportString[RetVal[[2]],"RawJSON"]];
]

(* Post a nanorun using previous data and clustering parameters *)
Options[RunNano]={Results->None};
RunNano[NanoHandle_,OptionsPattern[]]:=Module[{req,url,RetVal,ResultString=""},
	If[NanoHandle===Null,Message[NanoError::handle,HoldForm[NanoHandle]];Return[]];
	If[SubsetQ[{"None","All","ID","SI","RI","FI","DI","MD"},ToString/@Flatten[{OptionValue[Results]}]]==False,
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

End[] (* End Private Context *)

Protect[RunNano]
Protect[LoadData]
Protect[RunStreamingData]

EndPackage[]