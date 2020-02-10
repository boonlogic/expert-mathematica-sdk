(* ::Package:: *)

BeginPackage["UpdateNano`"]


Begin["`Private`"]


(* Call this to update the nano rest API *)
Module[{curr,app},
curr=FileNameJoin[{Directory[],"NanoREST.zip"}];
app=FileNameJoin[{$UserBaseDirectory,"Applications"}];
If[MemberQ[FileNames["*",app],FileNameJoin[{app,"NanoREST"}]],DeleteDirectory[FileNameJoin[{app,"NanoREST"}],DeleteContents->True]];
ExtractArchive[curr,app];
PrintTemporary["Running Tests...."];
Print[TestReport@FileNameJoin[{app,"NanoREST", "UnitTests.wlt"}]]
(*Print["Files updated"];*)
]


End[]


EndPackage[]
