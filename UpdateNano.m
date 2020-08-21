(* ::Package:: *)

BeginPackage["UpdateNano`"]


LoadPackage::usage="LoadPackage[packageName] moves the custom package to a default location for mathematica to find"
RunTests::usage="RunTests[packageName] runs the UnitTests.wlt file in the package (if applicable)"


Begin["`Private`"]


(* Call this to update the nano rest API *)
LoadPackage[packageName_]:=Module[{curr,app},
curr=CreateArchive[FileNameJoin[{Directory[],packageName}],OverwriteTarget->True];
app=FileNameJoin[{$UserBaseDirectory,"Applications"}];
If[MemberQ[FileNames["*",app],FileNameJoin[{app,packageName}]],DeleteDirectory[FileNameJoin[{app,packageName}],DeleteContents->True]];
ExtractArchive[curr,app];
DeleteFile[curr];
Print["Files updated"];
]


RunTests[packageName_]:=Module[{app=FileNameJoin[{$UserBaseDirectory,"Applications"}];},
PrintTemporary["Running Tests...."];
If[FindFile[FileNameJoin[{app,packageName, "UnitTests.wlt"}]],
Print[TestReport@FileNameJoin[{app,packageName, "UnitTests.wlt"}]]
,
Print["No tests found"];
]
]


End[]


EndPackage[]
