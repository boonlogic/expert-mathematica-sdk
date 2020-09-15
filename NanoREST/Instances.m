(* Wolfram Language Package *)

BeginPackage["NanoREST`"]

(* INSTANCES *)
OpenNano::usage="OpenNano[Label] starts a nano using the default user's credentials and returns the NanoHandle
\nOpenNano[Label,User] starts a nano with the given label using the user's credentials and returns the NanoHandle"

CloseNano::usage="CloseNano[NanoHandle] stops the nano associated with the NanoHandle and closes the connection"

NanoList::usage="NanoList[NanoHandle] returns a list names for the allocated nanos"

ExistsQ::usage="ExistsQ[NanoHandle] tests to see whether the given nanohandle still poitns to an active nano"

SaveNano::usage="SaveNano[NanoHandle,Filename] saves the nano in a .bn file"

LoadNano::usage=""

Begin["`Private`"] (* Begin Private Context *) 

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

End[] (* End Private Context *)

Protect[OpenNano]
Protect[CloseNano]
Protect[NanoList]
Protect[ExistsQ]
Protect[SaveNano]

EndPackage[]