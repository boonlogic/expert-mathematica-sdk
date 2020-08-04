(* ::Package:: *)

BeginPackage["UpdateNano`"]


Begin["`Private`"]


(* Call this to update the nano rest API *)
Module[{curr,app,tests},
curr=CreateArchive[FileNameJoin[{Directory[],"NanoREST"}],OverwriteTarget->True];
app=FileNameJoin[{$UserBaseDirectory,"Applications"}];
If[MemberQ[FileNames["*",app],FileNameJoin[{app,"NanoREST"}]],DeleteDirectory[FileNameJoin[{app,"NanoREST"}],DeleteContents->True]];
ExtractArchive[curr,app];
PrintTemporary["Running Tests...."];
(*tests=FileNames[All,FileNameJoin[{app,"NanoREST", "UnitTests"}]];
Print[TestReport/@tests]*)
(*Print[TestReport@FileNameJoin[{app,"NanoREST","UnitTests-all.wlt"}]]*)
]


End[]


EndPackage[]
