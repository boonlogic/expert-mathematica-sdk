(* ::Package:: *)

BeginPackage["UpdateNano`"]


Begin["`Private`"]


(* Call this to update the nano rest API *)
Module[{curr,app},
curr=CreateArchive[FileNameJoin[{Directory[],"NanoREST"}],OverwriteTarget->True];
app=FileNameJoin[{$UserBaseDirectory,"Applications"}];
ExtractArchive[curr,app,OverwriteTarget->True];
PrintTemporary["Running Tests...."];
Print[TestReport@FileNameJoin[{app,"NanoREST", "UnitTests.wlt"}]]
]


End[]


EndPackage[]
