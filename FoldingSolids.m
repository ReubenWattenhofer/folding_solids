(* ::Package:: *)

(*Converts a polyhedral graph into the 3D solid it represents.  Very convenient :)
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next.  Winding order (clockwise vs counterclockwise) is irrelevant for
			every face EXCEPT THE FIRST.  The first face is assumed to wind clockwise.
	edgeLength - length of each edge
	filePath - can be absolute or relative, must be a string formatted so Mathematica can read it. If relative, Mathematica will assume root is Documents directory (for Windows anyway)
	verbose - number of frames per fold animation.  0 indicates no animation should be created
	alignFreq - how often to align first face with xy plane 
				0: never
				-1: at end of folding
				x >= 1: every x folds 

	Output:
	{Graphics3D object (representation of polyhedral graph), coordinates of every face (rounded to 2 decimal places) }
*)
polyhedralGraphToShape[faces_, edgeLength_, filePath_, verbose_, alignFreq_]:=
	Module[{netInfo, ordered, adjacentFaces, duplicates, tempFaces, duplicateVertices},
		(*unfold the polyhedral graph*)
		netInfo = unfold[faces];
		(*order the faces and duplicate edges*)
		ordered = orderNetInfo[netInfo[[1]], netInfo[[3]]];

		(*duplicateVertices = netInfo[[4]];*)
		tempFaces = ordered[[1]];
		adjacentFaces = netInfo[[2]];
		duplicates = ordered[[2]];
		
		(*Hand the work off*)
		Return[netToShape[tempFaces, adjacentFaces, duplicates, edgeLength, filePath, verbose, alignFreq]];
	];


(*If you don't have a polyhedral graph, call this instead.  Requires more work on the user's end though.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - dual of input net, where each face is a vertex whose value is its position in the faces set
	duplicates - duplicate edges in graph, with endpoints in clockwise order
	edgeLength - length of each edge
	filePath - can be absolute or relative, must be a string formatted so Mathematica can read it. If relative, Mathematica will assume root is Documents directory (for Windows anyway)
	verbose - number of frames per fold animation.  0 indicates no animation should be created
	alignFreq - how often to align first face with xy plane 
				0: never
				-1: at end of folding
				x >= 1: every x folds 
	
	Output:
	{Graphics3D object (representation of folded net), coordinates of every face (rounded to 2 decimal places) }
*)
netToShape[faces_, adjacentFaces_, duplicates_, edgeLength_, filePath_, verbose_, alignFreq_]:=
	Module[{result, tempFaces, tempAdjacentFaces, tempDuplicates, output},
		(*format the net*)
		result = formatNet[faces, adjacentFaces, duplicates];
		
		output = False;
		If[result[[4]],
			tempFaces = result[[1]];
			tempAdjacentFaces = result[[2]];
			tempDuplicates = result[[3]];

			output = findCoordinates[tempFaces, tempAdjacentFaces, tempDuplicates, getDuplicateVertexList[tempDuplicates], Max[tempFaces, 2], edgeLength, filePath, verbose, alignFreq]; 			
		];
		
		(*pass the work off*)
		Return[output];		
	];


(*Finds all the duplicate vertices in a net.  This is done automatically in polyhedralGraphToShape, so this function is only useful if calling netToShape directly.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	duplicates - duplicate edges in graph, with endpoints in clockwise order
	
	Output:
	list of which vertices are duplicates { {1,2}, {2,3}, {1,4}, ...}
*)
getDuplicateVertexList[duplicateEdges_]:=
	Module[{duplicateVertices, dup1, dup2, proceed1, proceed2, i, j},
		(*Print[duplicateEdges];*)
		
		duplicateVertices = {};
		(*Print[Length[duplicateEdges]];*)
		Do[
			(
			dup1 = DeleteDuplicates[{duplicateEdges[[j]][[1]][[1]], duplicateEdges[[j]][[2]][[2]] }];
			dup2 = DeleteDuplicates[{duplicateEdges[[j]][[1]][[2]], duplicateEdges[[j]][[2]][[1]] }];			
			
(*			Print[dup1];
			Print[dup2];
			Print[Length[dup1]];
			Print[Length[dup2]];
*)
			
			proceed1 = True;
			proceed2 = True;
			If[Length[dup1] < 2, (proceed1 = False)];
			If[Length[dup2] < 2, (proceed2 = False)];
			Do[
				(
				
				If[Length[Intersection[duplicateVertices[[i]], dup1] ] == 2, (proceed1 = False) ];
				If[Length[Intersection[duplicateVertices[[i]], dup2] ] == 2, (proceed2 = False) ];
				
				),
				{i, 1, Length[duplicateVertices]}
			];
			
(*			Print[proceed1];
			Print[proceed2];*)
			If[proceed1, (AppendTo[duplicateVertices, dup1])];
			If[proceed2, (AppendTo[duplicateVertices, dup2])];
			(*Print[duplicateVertices];*)
			),
			{j, 1, Length[duplicateEdges]}
		];
		
		(*Print[duplicateVertices];*)
		Return[duplicateVertices];		
	];

	
(*Formats a net so that the algorithm will be able to fold it.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - dual of input net, where each face is a vertex whose value is its position in the faces set
	duplicates - duplicate edges in graph, with endpoints in clockwise order
	
	Output:
	{faces, adjacentFaces, duplicates} -- ie a new net if necessary, otherwise unchanged
*)
formatNet[faces_, adjacentFaces_, duplicates_]:=
	Module[{tempAdjacentFaces, tempDuplicates, tempFaces, notSafe, info, split, faceInfo, newFaces, oldFaces, tempNewFaces, tempOldFaces, newEdge, newDuplicateEdges,
		i,j, connectEdge, longLostCousin, looseVertices, replaceMe, replaced, replacedWith, index, temp, p1, p2, f, output, count, count2, succeeded},
		(*seed the random generator*)
		SeedRandom[];
		tempFaces = faces;
		tempAdjacentFaces = adjacentFaces;
		tempDuplicates = duplicates;
		
		notSafe = True;
		succeeded = True;
		While[notSafe,
			
			notSafe = False;
			(*find number of vertices with degree >= 5*)
			count = 0;
			Do[
				(
				If[degreeOfVertex[tempFaces, j] >= 5, count = count + 1];
				),
				{j, 1, Length[tempFaces]}
			];
			
			(*find vertices with degree 5 or greater -- they must be dealt with*)
			Do[
				(
					If[degreeOfVertex[tempFaces, j] >= 5,
						(
					(*	Print["-------------------------"];
						Print[StringForm["vertex ``", j]];*)
						
						(*TODO: uncomment this once code is fixed*)
						notSafe = True;
						
						(*get the information we need to split*)
						info = findFaceToSplit[faces, adjacentFaces, duplicates, j];
					(*	Print[info];*)
						If[info == {}, Continue[], (*if nothing is returned, then we can't deal with this vertex yet*)
							(
							(*randomly pick one of the faces*)
							faceInfo = RandomChoice[info];

					(*		Print["info"];
							Print[info];
							Print["Face info"];
							Print[faceInfo];*)

							(*get the faces that will be attached to the face when it splits from the net*)
							split = getNetHalves[tempAdjacentFaces, faceInfo[[1]], faceInfo[[2]] ];
							newFaces = split[[1]];
							oldFaces = split[[2]];
							
				(*			Print[StringForm["new faces: ``", newFaces]];
							Print[StringForm["old faces: ``", oldFaces]];*)
							
							(*holds the changed vertices -- ie the new/unchnaged faces, not the indices*)
							tempNewFaces = {};
							Do[
								(
								AppendTo[tempNewFaces, tempFaces[[newFaces[[i]]]] ];								
								),
								{i, 1, Length[newFaces]}
							];
							tempOldFaces = {};
							Do[
								(
								AppendTo[tempOldFaces, tempFaces[[oldFaces[[i]]]] ];								
								),
								{i, 1, Length[oldFaces]}
							];

						(*	Print["tempNewFaces"];
							Print[tempNewFaces];
							Print["tempOldFaces"];
							Print[tempOldFaces];*)
							
							
							(*choose one of the duplicate edges to connect*)
							connectEdge = RandomChoice[faceInfo[[4]] ];
							
					(*		Print[StringForm["connectEdge: ``", connectEdge]];*)

							(*replace the vertices that belong to connectEdge with its duplicate edge*)
							(*first find the duplicate edge*)
							longLostCousin = -1;
							Do[
								(
								If[MemberQ[duplicates[[i]], connectEdge],
									(
									If[duplicates[[i]][[1]] == connectEdge, longLostCousin = duplicates[[i]][[2]] ];
									If[duplicates[[i]][[2]] == connectEdge, longLostCousin = duplicates[[i]][[1]] ];
									)
								];
								),
								{i, 1, Length[duplicates]}
							];
				(*			Print[StringForm["longLostCousin: ``", longLostCousin]];*)
							
							(*go through each face now and replace the duplicate edge vertices*)
							tempNewFaces = ReplaceAll[connectEdge[[1]] -> longLostCousin[[2]] ] [tempNewFaces ];
							tempNewFaces = ReplaceAll[connectEdge[[2]] -> longLostCousin[[1]] ] [tempNewFaces ];

							(*both endpoints in the duplicate edge are potentially lost, so we can reuse those vertices*)
							(*TODO: why are the number of vertices for any net of a graph the same?*)
							looseVertices = {};			
							If[FreeQ[tempNewFaces , connectEdge[[1]], 2] && FreeQ[tempOldFaces, connectEdge[[1]], 2], AppendTo[looseVertices, connectEdge[[1]] ] ];				
							If[FreeQ[tempNewFaces , connectEdge[[2]], 2] && FreeQ[tempOldFaces, connectEdge[[2]], 2], AppendTo[looseVertices, connectEdge[[2]] ] ];
							
							
							(*Now, replace any vertices that originally belonged to the edge shared by both faces before the split*)
							replaceMe = {};
							(*keep track of which vertices we replaced with what*)
							replaced = {connectEdge[[1]], connectEdge[[2]]};
							replacedWith = {longLostCousin[[2]], longLostCousin[[1]]};
							If[MemberQ[tempNewFaces , faceInfo[[3]][[1]], 2], AppendTo[replaceMe, faceInfo[[3]][[1]]] ];							
							If[MemberQ[tempNewFaces , faceInfo[[3]][[2]], 2], AppendTo[replaceMe, faceInfo[[3]][[2]]] ];
							
							Do[
								(
								AppendTo[replaced, replaceMe[[i]]];
								AppendTo[replacedWith, looseVertices[[i]] ];
								tempNewFaces = ReplaceAll[replaceMe[[i]] -> looseVertices[[i]] ] [tempNewFaces ];

								),
								{i, 1, Length[looseVertices]}
							];
							
							
			(*				Print[StringForm["tempNewFaces: ``", tempNewFaces]];
							Print[StringForm["tempOldFaces: ``", tempOldFaces]];
							Print[StringForm["looseVertices: ``", looseVertices]];
							Print[StringForm["replaceMe: ``", replaceMe]];
							Print[StringForm["replaced: ``", replaced]];
							Print[StringForm["replacedWith: ``", replacedWith]];*)
							
							(*-------------------------*)
							(*update duplicates*)
							(*remove duplicate edges that we connected*)
							(*find index*)
							index = -1;
							Do[
								(
								If[MemberQ[tempDuplicates[[i]], connectEdge] && MemberQ[tempDuplicates[[i]], longLostCousin], (index = i; Break[]) ];
								),
								{i, 1, Length[tempDuplicates]}
							];
							(*ommit element at index*)
							temp = {};
							Do[
								(
								If[i != index, AppendTo[temp, tempDuplicates[[i]] ] ];
								),
								{i, 1, Length[tempDuplicates]}
							];
							tempDuplicates = temp;
							(*now update vertices*)
							(*old ones first*)
							(*NOTE: do not update tempFaces before calling this, or it won't know which face the original duplicate edge belonged to*)
							tempDuplicates = updateDuplicateEdges[tempFaces, newFaces, tempDuplicates, replaced, replacedWith];
							
							(*add the new duplicate edges we created*)
							(*find the new edge*)
							(*NOTE: faceInfo[[3]] is the splitting edge, but it's clockwise to the face that doesn't need it anymore, which is why we reverse it to make it
							clockwise to the adjacent face *)
							p1 = Position[replaced, faceInfo[[3]][[2]] ] [[1]][[1]];
							p2 = Position[replaced, faceInfo[[3]][[1]] ] [[1]][[1]];
							newEdge = {replacedWith[[p2]], replacedWith[[p1]]};							
							newDuplicateEdges = { {faceInfo[[3]][[2]], faceInfo[[3]][[1]]}, newEdge };
							AppendTo[tempDuplicates, newDuplicateEdges];
							
							
							(*-------------------------*)
							(*update adjacentFaces*)
							index = -1;
							Do[
								(
								If[MemberQ[tempAdjacentFaces[[i]], faceInfo[[1]] ] && MemberQ[tempAdjacentFaces[[i]], faceInfo[[2]] ], (index = i; Break[]) ];
								),
								{i, 1, Length[tempAdjacentFaces]}
							];
							(*remove connection from dual graph*)
							temp = {};
							Do[
								(
								If[i != index, AppendTo[temp, tempAdjacentFaces[[i]] ] ];
								),
								{i, 1, Length[tempAdjacentFaces]}
							];
							tempAdjacentFaces = temp;
							
							(*find new connection in adjacent faces and add it*)
							f = -1;
							Do[
								(
								If[isEdgeInFace[tempFaces[[i]], longLostCousin], (f = i; Break[]) ]
								),
								{i, 1, Length[tempFaces]}
							];
							AppendTo[tempAdjacentFaces, {faceInfo[[1]], f}];
							
							(*-------------------------*)
							(*update faces*)
							Do[
								(
								tempFaces = ReplacePart[tempFaces, newFaces[[i]] -> tempNewFaces[[i]] ];
								),
								{i, 1, Length[newFaces]}
							];														


						(*	Print[StringForm["tempFaces: ``", tempFaces]];
							Print[StringForm["tempAdjacentFaces: ``", tempAdjacentFaces]];
							Print[StringForm["tempDuplicates: ``", tempDuplicates]];*)
							
							)
						];
						
						)
					];
				),
				{j, 1, Max[faces, 2] } (*we're assuming that there are no gaps between the vertex numbers, and that the vertices are in fact numbers *)
			];
			
			(*find number of vertices with degree >= 5*)
			count2 = 0;
			Do[
				(
				If[degreeOfVertex[tempFaces, j] >= 5, count2 = count2 + 1];
				),
				{j, 1, Length[tempFaces]}
			];
			
			If[count2 >= count && count > 0, 
				(
				Print["Unable to format net correctly.  Try inputting a different net or polyhedral graph configuration."];
				notSafe = False;
				succeeded = False;
				)
			]

		];
		
		output = {tempFaces, tempAdjacentFaces, tempDuplicates, succeeded};
		Return[output];		
	];

(*updates list of duplicate edges with new vertices, but only for specified faces
	newFaces - index list of faces to update
	oldVertices - list of old vertices
	newVertices - corresponding list of new vertices to replace the old
*)
updateDuplicateEdges[faces_, newFaces_, duplicates_, oldVertices_, newVertices_ ]:=
	Module[{tempDuplicates, newEdge, pair, i, j, m},
		tempDuplicates = duplicates;
		Do[
			(
			Do[
				(
				Do[
					(
					If[isEdgeInFace[faces[[newFaces[[j]]]], duplicates[[i]][[1]] ],
						(
						newEdge = ReplaceAll[oldVertices[[m]] -> newVertices[[m]] ] [tempDuplicates[[i]][[1]] ];
						pair = {newEdge, tempDuplicates[[i]][[2]]};
						tempDuplicates = ReplacePart[tempDuplicates, i -> pair];
						)
					];
					If[isEdgeInFace[faces[[newFaces[[j]]]], duplicates[[i]][[2]] ],
						(
						newEdge = ReplaceAll[oldVertices[[m]] -> newVertices[[m]] ] [tempDuplicates[[i]][[2]] ];
						pair = {tempDuplicates[[i]][[1]], newEdge};
						tempDuplicates = ReplacePart[tempDuplicates, i -> pair];
						)
					];
					),
					{i, 1, Length[tempDuplicates]}
				];
				
				),
				{j, 1, Length[newFaces]}
			];
			),
			{m, 1, Length[oldVertices]}
		];

		Return[tempDuplicates];
	];



(*Returns the faces belonging to either side after the connection between f and f2 is removed
	Parameters:
	adjacentFaces - dual of net
	f - index of face 1
	f2 - index of face2
	
	Output:
	{{list of faces on side of f}, {list of faces on side of f2}}
*)
getNetHalves[adjacentFaces_, f_, f2_]:=
	Module[{index, i,j, tempAdjacentFaces, t, num, output},
		(*remove the first adjacent face from the dual graph; if it breaks the path to dupFace, then this is the face we want.  Otherwise it will be the second face*)
		(*find the connection*)
		(*number of vertices*)
		num = Max[adjacentFaces,2];
		
		index = -1;
		Do[
			(
			If[MemberQ[adjacentFaces[[i]], f ] && MemberQ[adjacentFaces[[i]], f2 ], (index = i; Break[]) ];
			),
			{i, 1, Length[adjacentFaces]}
		];
		(*remove connection from dual graph*)
		tempAdjacentFaces = {};
		Do[
			(
			If[i != index, AppendTo[tempAdjacentFaces, adjacentFaces[[i]] ] ];
			),
			{i, 1, Length[adjacentFaces]}
		];
		
		(*see if path exists from current face to f.  If so, add it to the left side.  Otherwise, add it to the right*)
		(*initialize output*)
		output = {{f},{f2}};

		Do[
			(
			If[j == f || j == f2, Continue[]];
			If[findPath[j, f, num, tempAdjacentFaces] != {}, AppendTo[output[[1]], j], AppendTo[output[[2]], j] ];			
			),
			{j, 1, num}
		];
		
		Return[output];
		
	];


(*Finds all faces that, when spliced onto a different portion of the net, will reduce the degree of a specified vertex and not bring any other vertex degree up to 5.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - dual of input net, where each face is a vertex whose value is its position in the faces set
	duplicates - duplicate edges in graph, with endpoints in clockwise order
	vertex - specified vertex in graph
	
	Output:
	{ {index of face, index of adjacent face sharing edge to split on, edge to split on (clockwise relative to face 1) , {list of edges to join on}}, ...}, or {} if unsuccessful
*)
findFaceToSplit[faces_, adjacentFaces_, duplicates_, vertex_]:=
	Module[{i,j, faceList, edges, tempEdges, temp, adjacent, tempAdjacentFaces, n, dup, dupFace, index, t, num, neighbor, splitEdges, output, p1, p2, edge1,
		shortEdges, halves, splitOff, stillHere, tempNewFaces, tempOldFaces, bundle1, bundle2, k},
		
		(*create a list of faces which have the vertex we want*)
		faceList = {};
		Do[
			(
				If[MemberQ[faces[[j]], vertex], AppendTo[faceList, j ] ];
			),
			{j, 1, Length[faces]}
		];

		(*find which faces have a duplicate edge that doesn't contain the vertex.  Keep track of the duplicate edges for each face*)
		temp = {};
		edges = {};
		Do[
			(
			
			Do[
				(
				(*skip edges which have vertex*)
				If[MemberQ[duplicates[[i]][[1]], vertex] || MemberQ[duplicates[[i]][[2]], vertex], Continue[] ];
				
				If[isEdgeInFace[faces[[faceList[[j]] ]], duplicates[[i]][[1]] ] || isEdgeInFace[faces[[faceList[[j]]]], duplicates[[i]][[2]] ],
					(
					If[FreeQ[temp, faceList[[j]] ],
						(
						AppendTo[temp, faceList[[j]] ];
						AppendTo[edges, {}];
						)
					];
					
					(*If[ isEdgeInFace[faces[[faceList[j]]], duplicates[[i]][[1]] ], AppendTo[edges[[Length[edges] ]], duplicates[[i]][[1]] ] ];
					If[ isEdgeInFace[faces[[faceList[j]]], duplicates[[i]][[2]] ], AppendTo[edges[[Length[edges] ]], duplicates[[i]][[2]] ] ];*)
					
					(*We'll need the matching edge for later, so don't sort anything yet*)
					AppendTo[edges[[Length[edges] ]], duplicates[[i]] ];
					
					)
				];
				),
				{i, 1, Length[duplicates]}
			];

			),
			{j, 1, Length[faceList]}
		];
		faceList = temp;


		(*Now, find which duplicate edge actually belongs to each face*)
		shortEdges = {};
		Do[
			(
			AppendTo[shortEdges, {}];
			Do[
				(
					If[ isEdgeInFace[faces[[faceList[[j]]]], edges[[j]][[i]][[1]] ], AppendTo[shortEdges[[Length[shortEdges] ]], edges[[j]][[i]][[1]] ] ];
					If[ isEdgeInFace[faces[[faceList[[j]]]], edges[[j]][[i]][[2]] ], AppendTo[shortEdges[[Length[shortEdges] ]], edges[[j]][[i]][[2]] ] ];
				),
				{i, 1, Length[edges[[j]] ] }
			];
				
			),
			{j, 1, Length[edges] }
		];
		(*edges = temp;*)
		
(*		Print[StringForm["faceList: ``", faceList]];
		Print[StringForm["edges: ``", edges]];
		Print[StringForm["shortEdges: ``", shortEdges]];*)
		
		(*for each face, find the one or two adjacent faces which share the vertex.  If there are two, determine which face connects the current face to a face with a duplicate edge.
		That will be the face to split from.  Sometimes, either face will work (I think) *)
		adjacent = {};
		Do[
			(
				(*AppendTo[adjacent, {}];*)
				
				temp = findTrueAdjacentFaces[faces, faceList[[j]] ];
				t = {};
				Do[
					(
					If[MemberQ[faces[[temp[[i]] ]], vertex ], AppendTo[t, temp[[i]]] ];				
					),
					{i, 1, Length[temp]} (*either 1 or 2*)
				];
				temp = t;
				
				If[Length[temp] == 1,
					(*one face? that's the face to split from, IF doing so separates the current face from the other face with the duplicate edge*)
					(
					neighbor  = {};
					If[faceSplitTest[faces, adjacentFaces, duplicates, shortEdges[[j]], faceList[[j]], temp[[1]]], neighbor  = temp[[1]] ];
					
					AppendTo[adjacent, neighbor];
					),
					
					(*two faces? determine which one connects the current face*)					
					(

					neighbor = {};
					If[faceSplitTest[faces, adjacentFaces, duplicates, shortEdges[[j]], faceList[[j]], temp[[1]]], neighbor = temp[[1]], neighbor = temp[[2]] ];
	
					(*add to list*)
					AppendTo[adjacent, neighbor];
									
					)
				];
			),
			{j, 1, Length[faceList]}
		];
		
		
		(*remove edges which, when joined with their duplicate, will create vertices with degree 5 or higher *)
		temp = {};
		Do[
			(
			(*initialize edge list for every face to nothing*)
			AppendTo[temp, {} ];
	
			(*pretend a split occured at the splitting edge -- get the split halves of the net*)
			halves = getNetHalves[adjacentFaces, faceList[[j]], adjacent[[j]]];
			splitOff = halves[[1]];
			stillHere = halves[[2]];
			(*holds the vertices -- ie the new/unchnaged faces, not the indices*)
			tempNewFaces = {};
			Do[
				(
				AppendTo[tempNewFaces, faces[[splitOff[[i]]]] ];								
				),
				{i, 1, Length[splitOff]}
			];
			tempOldFaces = {};
			Do[
				(
				AppendTo[tempOldFaces, faces[[stillHere[[i]]]] ];								
				),
				{i, 1, Length[stillHere]}
			];
			
		(*	Print["----------------------------"];
			Print[StringForm["j: ``",j]];
			Print[StringForm["faceList[[j]]: ``",faceList[[j]]]];
			Print[StringForm["adjacent[[j]]: ``",adjacent[[j]]]];
			Print[StringForm["splitOff: ``",splitOff]];
			Print[StringForm["stillHere: ``",stillHere]];
			Print[StringForm["tempNewFaces: ``",tempNewFaces]];
			Print[StringForm["tempOldFaces: ``",tempOldFaces]];
			Print[StringForm["edges: ``",edges]];*)


			Do[
				(
				(*figure out which half the first edge belongs to*)
				bundle1 = -1;
				bundle2 = -1;
				Do[
					(
					If[isEdgeInFace[tempNewFaces[[k]], edges[[j]][[i]][[1]] ], 
						(bundle1 = tempNewFaces; bundle2 = tempOldFaces)
					];					
					),
					{k, 1, Length[tempNewFaces]}
				];
				If[bundle1 == -1, (bundle1 = tempOldFaces; bundle2 = tempNewFaces)];
				
			(*	Print[StringForm["bundle1: ``",bundle1]];
				Print[StringForm["bundle2: ``",bundle2]];*)
										
				
				(*subtract 1 because the duplicate edge would count for 2 otherwise, and it won't when it's joined *)
				If[ degreeOfVertex[bundle1, edges[[j]][[i]][[1]][[1]] ] - 1 + degreeOfVertex[bundle2, edges[[j]][[i]][[2]][[2]] ] < 5
					 && degreeOfVertex[bundle1, edges[[j]][[i]][[1]][[2]] ] - 1 + degreeOfVertex[bundle2, edges[[j]][[i]][[2]][[1]] ] < 5,
					 
					 AppendTo[temp[[Length[temp] ]], edges[[j]][[i]] ]
				];
				),
				{i, 1, Length[edges[[j]] ] }
			];
			
			),
			{j, 1, Length[faceList]}
		];
		edges = temp;
		
		(*update shortEdges to reflect edges that were taken away*)
		temp = {};
		Do[
			(
			AppendTo[temp, {}];
			Do[
				(
					If[ isEdgeInFace[faces[[faceList[[j]]]], edges[[j]][[i]][[1]] ], AppendTo[temp[[Length[temp] ]], edges[[j]][[i]][[1]] ] ];
					If[ isEdgeInFace[faces[[faceList[[j]]]], edges[[j]][[i]][[2]] ], AppendTo[temp[[Length[temp] ]], edges[[j]][[i]][[2]] ] ];
				),
				{i, 1, Length[edges[[j]] ] }
			];
				
			),
			{j, 1, Length[edges] }
		];
		shortEdges = temp;
		
		
(*		(*now, mark the faces which no longer have any edges*)
		temp = {};
		Do[
			(
				If[edges[[j]] == {},
					(
					face
					 AppendTo[temp, faceList[[j]] ];
					 AppendTo[tempEdges, edges[[j]] ];
					)
				];
			),
			{j, 1, Length[faceList]}
		];
		faceList = temp;
*)		
		
		
		(*Now, for each face, find which edge the neighbor and the face share*)
		splitEdges = {};
		Do[
			(
			(*find neighbor vertices of the vertex within the current face*)
			n = getVertexNeighborsInFace[faces[[faceList[[j]]]], vertex];
			If[isEdgeInFace[faces[[faceList[[j]] ]], {vertex, n[[1]]} ] && isEdgeInFace[faces[[adjacent[[j]] ]], {vertex, n[[1]]} ],
				AppendTo[splitEdges, {vertex, n[[1]]}],
				AppendTo[splitEdges, {vertex, n[[2]]}]
			];
			),
			{j, 1, Length[faceList]}
		];
		
		(*order the split edges so they're clockwise relative to the first face (not the adjacent face)*)		
		Do[
			(
			(*find vertex positions*)
			p1 = Position[faces[[faceList[[j]] ]], splitEdges[[j]][[1]] ][[1]][[1]];
			p2 = Position[faces[[faceList[[j]] ]], splitEdges[[j]][[2]] ][[1]][[1]];
			
			(*order the edge properly*)
			edge1 = -1;
			If[(p1 < p2 && (p1 != 1 || p2 != Length[faces[[faceList[[j]] ]] ] )) || (p1 == Length[faces[[faceList[[j]] ]] ] && p2 == 1),
				(edge1 = {splitEdges[[j]][[1]], splitEdges[[j]][[2]]}),
				(edge1 = {splitEdges[[j]][[2]], splitEdges[[j]][[1]]}) ];
			splitEdges = ReplacePart[splitEdges, j -> edge1];			
			),
			{j, 1, Length[splitEdges]}
		];
		
		
		(*Now that we have all the data, arrange it nicely*)
		output = {};
		Do[
			(
			(*signal that this is a false face*)
			If[adjacent[[j]] == {}, Continue[]];
			If[shortEdges[[j]] == {}, Continue[]];
			
			AppendTo[output, {faceList[[j]], adjacent[[j]], splitEdges[[j]], shortEdges[[j]]}];
			),
			{j, 1, Length[faceList]}
		];
		
		Return[output];
		
	];


(*I wasn't sure what to call this. just a helper function because the same code is needed twice
	edges - duplicate edges belonging to f
	f - index of face
	adjacent - index of adjacent face
*)
faceSplitTest[faces_, adjacentFaces_, duplicates_, edges_, f_, adjacent_]:=
	Module[{yes, dup, dupFace, i, index, tempAdjacentFaces, t, num },
		yes = False;
		
		(*take any duplicate edge on the current face*)
		dup = -1;
		Do[
			(
			If[MemberQ[duplicates[[i]], edges[[1]] ],
				(
				If[duplicates[[i]][[1]] == edges[[1]], dup = duplicates[[i]][[2]], dup = duplicates[[i]][[1]] ];
				)
			];
				
			),
			{i, 1, Length[duplicates]}
		];
		
		(*find which face the duplicate edge belongs to*)
		dupFace = -1; (*index*)
		Do[
			(
			If[isEdgeInFace[faces[[i]], dup], (dupFace = i; Break[]) ];						
			),
			{i, 1, Length[faces]}
		];
		
		If[FreeQ[getNetHalves[adjacentFaces, f, adjacent][[1]], dupFace], yes = True ];
		(*If[findPath[f, dupFace, num, tempAdjacentFaces] == {}, yes = True ];*)

		(*add to list*)
		Return[yes];
	];
	


(*Returns True if an edge is in a face, False otherwise.  Edge does not have to be in clockwise order*)
isEdgeInFace[face_, edge_]:=
	Module[{yes},
		yes = False;
		If[MemberQ[face, edge[[1]]] && MemberQ[face, edge[[2]]], yes = True ];		
		Return[yes];
	];
	
	
(*Gets the degree of a vertex.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	vertex - vertex in graph
	
	Output:
	degree of vertex
*)
degreeOfVertex[faces_, vertex_] :=
	Module[{j, edges, neighbors, temp, add, i},
		(*Get every edge which contains the vertex*)
		edges = {};
		Do[
			(
			If[MemberQ[faces[[j]], vertex],
				(
				neighbors = getVertexNeighborsInFace[faces[[j]], vertex];
				AppendTo[edges, {vertex, neighbors[[1]]}];
				AppendTo[edges, {vertex, neighbors[[2]]}];
				)
			];
			),
			{j, 1, Length[faces]}
		];
		
		(*remove duplicate edges*)
		temp = {};
		Do[
			(
			
			add = True;
			Do[
				(
				If[MemberQ[temp[[i]], edges[[j]][[1]] ] && MemberQ[temp[[i]], edges[[j]][[2]] ], (add = False; Break[]) ];				
				),
				{i, 1, Length[temp]}
			];
			
			If[add, AppendTo[temp, edges[[j]] ] ];
			),
			{j, 1, Length[edges]}
		];		
		
		Return[Length[temp]];
	];


(* Takes a net with unordered face windings and makes every face clockwise.  Also corrects duplicate edge list so that edges are clockwise relative to their faces.
	Parameters:
	faces 		- vertices which comprise each face {{1,2,3,4},...} in the current net -- assumption is that each vertex is adjacent to the next.
					Winding order is irrelevant. (obviously)
	duplicates 	- list of which edges are duplicates { {{1,2}, {2,5}}, {edge3, edge4}, ...}
	
	Output:
	{faces, duplicates} -- data correctly formatted in clockwise order now.
*)
orderNetInfo[faces_, duplicates_]:=
	Module[{tempFaces, tempDuplicates, j, i, p1, p2, edge1, edge2},
		(*Fix faces first*)
		tempFaces = orderFaces[faces, 1, {}];
		tempDuplicates = {};
		
		(*find which face each duplicate edge belongs to, and make sure it's in the right order*)
		Do[
			(
			edge1 = -1;
			edge2 = -1;
			Do[
				(
				(*first edge*)
				If[FreeQ[findSharedVertices[{tempFaces[[i]], duplicates[[j]][[1]] }, 1, 2], -1],
					(
					(*find first vertex*)
					p1 = Position[tempFaces[[i]], duplicates[[j]][[1]][[1]] ][[1]][[1]];
					p2 = Position[tempFaces[[i]], duplicates[[j]][[1]][[2]] ][[1]][[1]];
					
					(*order the edge properly*)
					edge1 = -1;
					If[(p1 < p2 && (p1 != 1 || p2 != Length[tempFaces[[i]]] )) || (p1 == Length[tempFaces[[i]]] && p2 == 1),
						(edge1 = {duplicates[[j]][[1]][[1]], duplicates[[j]][[1]][[2]]}),
						(edge1 = {duplicates[[j]][[1]][[2]], duplicates[[j]][[1]][[1]]}) ];
					
					)
				];

				(*second edge*)
				If[FreeQ[findSharedVertices[{tempFaces[[i]], duplicates[[j]][[2]] }, 1, 2], -1],
					(
					(*find first vertex*)
					p1 = Position[tempFaces[[i]], duplicates[[j]][[2]][[1]] ][[1]][[1]];
					p2 = Position[tempFaces[[i]], duplicates[[j]][[2]][[2]] ][[1]][[1]];
		
					(*order the edge properly*)
					edge2 = -1;
					If[(p1 < p2 && (p1 != 1 || p2 != Length[tempFaces[[i]]] )) || (p1 == Length[tempFaces[[i]]] && p2 == 1),
						(edge2 = {duplicates[[j]][[2]][[1]], duplicates[[j]][[2]][[2]]}),
						(edge2 = {duplicates[[j]][[2]][[2]], duplicates[[j]][[2]][[1]]}) ];
					
					)
				];

				),
				{i, 1, Length[tempFaces]}
			];		
			
			(*add properly formatted edges to the new duplicate edge list*)
			AppendTo[tempDuplicates, {edge1, edge2}];
			
			),
			{j, 1, Length[duplicates]}
		];
				
		(*all done*)
		Return[{tempFaces, tempDuplicates}];
	]


(*Makes every face in a net (or arbitrary graph) wind clockwise. Recursive.
	Parameters:
	faces 	- vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next.
	f		- index of current face
	avoid	- list of faces to avoid recursing on, prevents infinite loops
	
	Output:	
	{faces} , all winding clockwise
*)
orderFaces[faces_, f_, avoid_]:=
	Module[{tempFaces, j, neighbors, tempAvoid, shared, p1, p2, reordered, edge, tempNeighbors},
		tempFaces = faces;
		tempAvoid = avoid;
		
		(*Assume current face is properly ordered*)
		
		
		(*find the neigbors of the current face *)
		tempNeighbors = findAdjacentFaces[faces, f];
		neighbors = {};
		Do[
			(
				If[FreeQ[findSharedVertices[tempFaces, f, tempNeighbors[[j]] ], -1], AppendTo[neighbors, tempNeighbors[[j]] ] ];
			),
			{j, 1, Length[tempNeighbors]}
		];
		
		(*reorder each neighbor face that hasn't already been reordered*)
		Do[
			(
			(*Avoid faces on avoid list*)
			If[MemberQ[tempAvoid, neighbors[[j]] ], Continue[] ];
			
			shared = findSharedVertices[tempFaces, f, neighbors[[j]] ];
			
			(*find first vertex*)
			p1 = Position[tempFaces[[f]], shared[[1]] ][[1]][[1]];
			p2 = Position[tempFaces[[f]], shared[[2]] ][[1]][[1]];

			(*reorder the neighbor face*)
			reordered = -1;
			edge = -1;
			If[(p1 < p2 && (p1 != 1 || p2 != Length[tempFaces[[f]]] )) || (p1 == Length[tempFaces[[f]]] && p2 == 1), (edge = {shared[[1]], shared[[2]]}), (edge = {shared[[2]], shared[[1]]}) ];
			reordered = reorderFace[tempFaces[[neighbors[[j]]]], edge];
			tempFaces = ReplacePart[tempFaces, neighbors[[j]] -> reordered ];
				
			),
			{j, 1, Length[neighbors]}
		];

		
		(*update information before recursion*)
		AppendTo[tempAvoid, f];
		
		Do[
			(
			(*Avoid faces on avoid list*)
			If[MemberQ[tempAvoid, neighbors[[j]] ], Continue[]];
						
			tempFaces = orderFaces[tempFaces, neighbors[[j]], tempAvoid];
			
			),
			{j, 1, Length[neighbors] }
		];


		Return[tempFaces];
		
	];


(*Reorders a face so that it's in clockwise order
	Parameters:
	face - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next
	edge - edge in an adjacent face which belongs to both faces, assumed to be in clockwise order!
	
	Output:
	face, in clockwise order
*)
reorderFace[face_, edge_]:=
	Module[{tempFace, nextVertex},
		(*initialize new ordering*)
		tempFace = {edge[[2]], edge[[1]]};
		
		While[ Length[tempFace] < Length[face],
			
				(*get neighbors of latest vertex in new face*)
				nextVertex = getVertexNeighborsInFace[face, tempFace[[Length[tempFace]]]];
				(*add the neighbor which isn't already in the new face*)				
				If[FreeQ[tempFace, nextVertex[[1]]], AppendTo[tempFace, nextVertex[[1]]], AppendTo[tempFace, nextVertex[[2]]] ];				
			
		];
		
		Return[tempFace];
	];



(* This function "unfolds" a polyhedral graph into a net.
	NOTE: the output is unordered, and is not ready to be used yet!
	
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} in polyhedral graph -- assumption is that each vertex is adjacent to the next.  Winding order is irrelevant.
	
	Output:
	{faces, adjacentFaces, duplicates, duplicate vertices} -- see documentation of recursiveUnfold for explanation and formatting of each sublist.	
*)
unfold[faces_]:=
	Module[{result, i, j, k, m, newFaces, tempDuplicateVertices, tempDuplicates, v1, v2, d1, d2, duplicateList, output, lilList1, lilList2, proceed,
		date, dateString, strm},
		
(*		date = DateList[TimeZone->$TimeZone];
		dateString = ToString[StringForm["``_``_``_``_``", date[[1]], date[[2]], date[[3]], date[[4]], date[[5]]]]; 
		
		strm = OpenWrite["recursiveUnfold_Mathematica_"<>dateString<>".log"];*)
		(*strm = OpenWrite["output.log"];*)
		strm = Null;
		
		result = recursiveUnfold[faces, 1, {}, {}, {}, {}, {}, 1, strm];
		
(*		Close[strm];*)


		newFaces = result[[1]];
		tempDuplicateVertices = result[[6]];
		tempDuplicates = {};
		
		(*find shared edges*)		
		Do[
			(
			Do[
				If[j == m, Continue[]];
				(
				v1 = -1;
				v2 = -1;
				d1 = -1;
				d2 = -1;
				
				(*loop through every vertex in the face and get its duplicates*)
				duplicateList = {};
				Do[
					(
					AppendTo[duplicateList, getDuplicateVertices[tempDuplicateVertices, newFaces[[j]][[i]] ] ];
					AppendTo[duplicateList[[Length[duplicateList]]], newFaces[[j]][[i]] ];
					),
					{i, 1, Length[newFaces[[j]] ] }
				];
			(*	Print["duplicateList"];
				Print[duplicateList];*)
				
				(*now, find the vertices that are shared*)
				Do[
					(
					Do[
						(
						If[MemberQ[newFaces[[m]], duplicateList[[i]][[k]] ],
							(
							If[ v1 == -1,
								(
								v1 = newFaces[[j]][[i]];
								d1 = duplicateList[[i]][[k]];
								),
								
								(
								v2 = newFaces[[j]][[i]];
								d2 = duplicateList[[i]][[k]];
								)
							];
							)
						];
						),
						{k, 1, Length[duplicateList[[i]] ] }
					];

					),
					{i, 1, Length[newFaces[[j]] ] }
				];
				
				If[v1 != -1 && v2 != -1 && (v1 != d1 || v2 != d2),
					(
					(*make sure duplicates aren't already in the list*)
					proceed = True;
					Do[
						(
						
						lilList1 = DeleteDuplicates[{v1, v2, d1, d2}];
						lilList2 = DeleteDuplicates[{tempDuplicates[[i]][[1]][[1]], tempDuplicates[[i]][[1]][[2]], tempDuplicates[[i]][[2]][[1]], tempDuplicates[[i]][[2]][[2]] }];
						
						If[Length[lilList1] != Length[lilList2], Continue[]]; (*obviously not going to be the same in that case*)
						If[Length[Intersection[lilList1, lilList2] ] == Length[lilList1], (proceed = False; Break[]) ];
						
						),
						{i, 1, Length[tempDuplicates]}
					];
					
					If[proceed,
						(
					(*	Print[StringForm["		duplicate Edges: ``", {{v1, v2}, {d1, d2}}]];*)
						(*add the edges now*)
						AppendTo[tempDuplicates, {{v1, v2}, {d1, d2}}];
						)
					];
					
					)
				];
				
			
				),
				{m, 1, Length[newFaces]}
			];			
			),
			{j, 1, Length[newFaces]}
		];
				
	(*	Print[result[[6]]];
		Print[tempDuplicates];*)
		
		output = {newFaces, result[[4]], tempDuplicates};(*, tempDuplicateVertices};*)		
		Return[output];
	];
	

(* This function "unfolds" a polyhedral graph into a net.  Recursive.
	Parameters:
	polyhedralFaces 	- vertices which comprise each face {{1,2,3,4},...} in polyhedral graph -- assumption is that each vertex is adjacent to the next.
							Winding order is irrelevant.
	parent 				- parent of current face
	ancestors 			- index list of already-unfolded faces
	faces 				- vertices which comprise each face {{1,2,3,4},...} in the current net -- assumption is that each vertex is adjacent to the next.
							Winding order is irrelevant.
	adjacentFaces 		- dual of net graph, list of which faces are adjacent to each other {{1,2}, {2,3}, {1,3}, ...}.  Order of edges and endpoints is irrelevant.
	duplicates 			- list of which edges are duplicates { {{1,2}, {2,5}}, {edge3, edge4}, ...}
	duplicateVertices 	- list of which vertices are duplicates { {1,2}, {2,3}, {1,4}, ...}
	f					- index of current face being unfolded
	strm 				- output stream for logging purposes
	
	Output:
	{ polyhedralFaces, ancestors, faces, adjacentFaces, duplicates, duplicateVertices }
			
*)
recursiveUnfold[polyhedralFaces_, parent_, ancestors_, faces_, adjacentFaces_, duplicates_, duplicateVertices_, f_, strm_] :=
	Module[{i, j, k, tempPolyhedralFaces, tempAncestors, tempFaces, tempAdjacentFaces, tempDuplicates, output, result, adjacent, sharedVertices, replaceVertices, newVertex,
		newVertices, newFace, unused, neighbors1, neighbors2, equivalent, ancestor2, current2, tempDuplicateVertices, sharedVertex, duplicateList, v1, v2, d1, d2, tempParent, trueAdjacent,
		date, dateString },
		
		tempPolyhedralFaces = polyhedralFaces;
		tempAncestors = ancestors;
		tempFaces = faces;
		tempAdjacentFaces = adjacentFaces;
		tempDuplicates = duplicates;
		tempDuplicateVertices = duplicateVertices;
		tempParent = parent;


		(*initialize output to nothing*)
		output = {};
		
		(*get adjacent faces*)
		adjacent = findAdjacentFaces[polyhedralFaces, f];

	(*	Write[strm,"-----------------------------------------------"];
		Write[strm,StringForm["polyhedral face: ``", f]];
		Write[strm,StringForm["adjacent faces: ``", adjacent]];
		Write[strm, StringForm["ancestors: ``", tempAncestors]];*)
		
		(*see if any adjacent faces are ancestors (other than parent).  If so, create new vertices to remove the adjacency *)
		Do[
			(
			If[MemberQ[tempAncestors[[1;;Length[tempAncestors]-1 ]], adjacent[[j]] ],
				(
				sharedVertices = findSharedVertices[tempPolyhedralFaces, adjacent[[j]], f ];
				
				(*not quite sure why this is necessary, but it is.  TODO: figure out why*)
				(*If[sharedVertices[[1]] == -1 && sharedVertices[[2]] == -1, Continue[] ];*)
				(**)
				
		(*		Write[strm, StringForm["adjacent face: ``", adjacent[[j]]]];
				Write[strm, StringForm["sharedVertices: ``", sharedVertices]];
				
				Write[strm, StringForm["parent: ``", tempParent ] ];
				Write[strm, StringForm["face of parent: ``", tempParent ] ];*)
				
				(*exclude vertices shared by the parent, and vertices that aren't actually vertices (ie -1) *)
				replaceVertices = {};
				If [FreeQ[tempPolyhedralFaces[[tempParent]], sharedVertices[[1]] ] && sharedVertices[[1]] != -1, AppendTo[replaceVertices, {sharedVertices[[1]], 1}] ];
				If [FreeQ[tempPolyhedralFaces[[tempParent]], sharedVertices[[2]] ] && sharedVertices[[2]] != -1, AppendTo[replaceVertices, {sharedVertices[[2]], 2}] ];
				
				(*Write[strm, StringForm["replaceVertices: ``", replaceVertices]];*)
				
				(*create new vertices.  There may not be anything to replace*)
				newVertices = sharedVertices;
				Do[
					(
					newVertex = Max[tempPolyhedralFaces, 2] + 1;
					
					(*Write[strm, StringForm["		newVertex: ``", newVertex]];*)
					
					(*update list of duplicate vertices*)
					AppendTo[tempDuplicateVertices, {newVertices[[replaceVertices[[i]][[2]]]], newVertex}];
					
					newVertices = ReplacePart[newVertices, replaceVertices[[i]][[2]] -> newVertex];
					
					(*replace vertex on original face first*)
					newFace = ReplacePart[tempPolyhedralFaces[[f]], Position[tempPolyhedralFaces[[f]], replaceVertices[[i]][[1]] ] -> newVertex];
					tempPolyhedralFaces = ReplacePart[tempPolyhedralFaces, f -> newFace];
					
				(*	Write[strm, "new face:"];					
					Write[strm, newFace];*)
									
					Do[
						(
						(*replace vertices only for faces which haven't been used yet*)
					(*	Write[strm, "**********"];
						Write[strm, StringForm["		iteration: ``", k]];
						Write[strm, StringForm["		face index: ``", adjacent[[k]] ]];
						Write[strm, StringForm["		face to replace vertex with: ``", tempPolyhedralFaces[[adjacent[[k]]]] ]];
						Write[strm, StringForm["		vertex to replace: ``", replaceVertices[[i]][[1]] ]];*)
					(*	If[FreeQ[tempPolyhedralFaces[[adjacent[[k]]]], replaceVertices[[i]][[1]] ] , Write[strm, "does not contain the vertex"] ];					
						If[MemberQ[tempAncestors, adjacent[[k]] ] , Write[strm, "face is an ancestor, untouchable"] ];			*)		

						If[FreeQ[tempAncestors, adjacent[[k]] ] && MemberQ[tempPolyhedralFaces[[adjacent[[k]]]], replaceVertices[[i]][[1]] ],
						(*	Write[strm, StringForm["			k: ``", k ]];*)
							(*tempPolyhedralFaces = ReplacePart[tempPolyhedralFaces[[k]], Position[tempPolyhedralFaces[[k]], replaceVertices[[i]] ] -> newVertex];*)
							newFace = ReplacePart[tempPolyhedralFaces[[adjacent[[k]]]], Position[tempPolyhedralFaces[[adjacent[[k]]]], replaceVertices[[i]][[1]] ] -> newVertex];
							tempPolyhedralFaces = ReplacePart[tempPolyhedralFaces, adjacent[[k]] -> newFace];
						(*	Write[strm, StringForm["			newFace: ``", newFace]];*)
						];
						
						),
						{k, 1, Length[adjacent] }
					];
					
					),
					{i, 1, Length[replaceVertices] }
				];
				
				(*update list of duplicates -- note that the edge endpoints are unordered!! They will be ordered once the faces are ordered properly*)
		(*		Write[strm, StringForm["newVertices: ``", newVertices ]];
				Write[strm, StringForm["new faces: ``", tempPolyhedralFaces ]];*)
				
				
(*				(*find shared edges*)
				v1 = -1;
				v2 = -1;
				d1 = -1;
				d2 = -1;
				
				(*loop through every vertex in the face and get its duplicates*)
				duplicateList = {};
				Do[
					(
					AppendTo[duplicateList, getDuplicateVertices[tempDuplicateVertices, tempPolyhedralFaces[[f]][[i]] ] ];
					AppendTo[duplicateList[[Length[duplicateList]]], tempPolyhedralFaces[[f]][[i]] ];
					),
					{i, 1, Length[tempPolyhedralFaces[[f]] ] }
				];
				Print["duplicateList"];
				Print[duplicateList];
				
				(*now, find the vertices that are shared*)
				Do[
					(
					Do[
						(
						If[MemberQ[tempPolyhedralFaces[[adjacent[[j]]]], duplicateList[[i]][[k]] ],
							(
							If[ v1 == -1,
								(
								v1 = tempPolyhedralFaces[[f]][[i]];
								d1 = duplicateList[[i]][[k]];
								),
								
								(
								v2 = tempPolyhedralFaces[[f]][[i]];
								d2 = duplicateList[[i]][[k]];
								)
							];
							)
						];
						),
						{k, 1, Length[duplicateList[[i]] ] }
					];

					),
					{i, 1, Length[tempPolyhedralFaces[[f]] ] }
				];
				
				If[v1 != -1 && v2 != -1,
					(
					Print[StringForm["		duplicate Edges: ``", {{v1, v2}, {d1, d2}}]];
					(*add the edges now*)
					AppendTo[tempDuplicates, {{v1, v2}, {d1, d2}}];
					)
				];*)
							
(*				(*If two endpoints are shared, then it's easy to find the duplicate edges*)
				If[FreeQ[sharedVertices, -1], AppendTo[tempDuplicates, {sharedVertices, newVertices}],
				(*If they're not, then that means that one or two of them is a new vertex, and we have to find its corresponding vertex*)
				(
					(*where any vertices replaced?*)
					If[replaceVertices != {},
						(
						(*get neighbors of the vertex in the ancestor face*)					
						neighbors1 = getVertexNeighborsInFace[tempPolyhedralFaces[[adjacent[[j]]]], replaceVertices[[1]][[1]] ];
						Print[StringForm["		newVertices: ``", newVertices ]];

						Print[StringForm["		replaceVertices: ``", replaceVertices]];
						Print["		neighbors of " <> ToString[replaceVertices[[1]][[1]]] <> " in face " <> ToString[adjacent[[j]]] <> ", " <> ToString[tempPolyhedralFaces[[adjacent[[j]]]]] ];
						Print[neighbors1 ];
						(*get neighbors of the vertex in the current face*)
						neighbors2 = getVertexNeighborsInFace[tempPolyhedralFaces[[f]], newVertices[[replaceVertices[[1]][[2]]]] ];
						Print["		neighbors of " <> ToString[newVertices[[replaceVertices[[1]][[2]]]] ] <> " in face " <> ToString[f] <> ", " <> ToString[tempPolyhedralFaces[[f]]] ];
						Print[neighbors2 ];
						(*find duplicates of these vertices now.  We only need to do this for one side.  Choose ancestor face bc fun *)					
						equivalent = {getDuplicateVertices[tempDuplicateVertices, neighbors1[[1]] ], getDuplicateVertices[tempDuplicateVertices, neighbors1[[2]] ]};
						Print[StringForm["		equivalent: ``", equivalent]];
						
						(*find the second vertex in both faces*)
						ancestor2 = -1;
						current2 = -1;
						
						If[MemberQ[equivalent[[1]], neighbors2[[1]] ], (ancestor2 = neighbors1[[1]]; current2 = neighbors2[[1]]; ) ];
						If[MemberQ[equivalent[[1]], neighbors2[[2]] ], (ancestor2 = neighbors1[[1]]; current2 = neighbors2[[1]]; ) ];
						If[MemberQ[equivalent[[2]], neighbors2[[1]] ], (ancestor2 = neighbors1[[2]]; current2 = neighbors2[[1]]; ) ];
						If[MemberQ[equivalent[[2]], neighbors2[[2]] ], (ancestor2 = neighbors1[[2]]; current2 = neighbors2[[2]]; ) ];
						
						Print[StringForm["		duplicate Edges: ``", {{replaceVertices[[1]][[1]], ancestor2}, {newVertices[[replaceVertices[[1]][[2]]]], current2}}]];
	
						(*add the edges now*)
						AppendTo[tempDuplicates, {{replaceVertices[[1]][[1]], ancestor2}, {newVertices[[replaceVertices[[1]][[2]]]], current2}}];
						),
					(*-----------------------------*)
					(*No replaced vertices?*)
					(*-----------------------------*)
						(
						(*find the vertex that belongs in both faces*)
						sharedVertex = -1;
						If[sharedVertices[[1]] == -1, sharedVertex = sharedVertices[[2]], sharedVertex = sharedVertices[[1]] ];

						(*sharedVertex = -1? Things get trickier; it means that the edge that used to make the face adjacent was overwritten a few loops back, so we need to find it again *)
						If[sharedVertex == -1,
							(
							v1 = -1;
							v2 = -1;
							d1 = -1;
							d2 = -1;
							
							(*loop through every vertex in the face and get its duplicates*)
							duplicateList = {};
							Do[
								(
								AppendTo[duplicateList, getDuplicateVertices[tempDuplicateVertices, tempPolyhedralFaces[[f]][[i]] ] ];
								),
								{i, 1, Length[tempPolyhedralFaces[[f]] ] }
							];
							
							(*now, find the vertices that are shared*)
							Do[
								(
								Do[
									(
									If[MemberQ[tempPolyhedralFaces[[adjacent[[j]]]], duplicateList[[i]][[k]] ],
										(
										If[ v1 == -1,
											(
											v1 = tempPolyhedralFaces[[f]][[i]];
											d1 = duplicateList[[i]][[k]];
											),
											
											(
											v2 = tempPolyhedralFaces[[f]][[i]];
											d2 = duplicateList[[i]][[k]];
											)
										];
										)
									];
									),
									{k, 1, Length[duplicateList[[i]] ] }
								];

								),
								{i, 1, Length[tempPolyhedralFaces[[f]] ] }
							];
							
							Print[StringForm["		duplicate Edges: ``", {{v1, v2}, {d1, d2}}]];
							(*add the edges now*)
							AppendTo[tempDuplicates, {{v1, v2}, {d1, d2}}];							
							
							),
							(*happy(er) case*)
							(
							(*get neighbors of the vertex in the ancestor face*)					
							neighbors1 = getVertexNeighborsInFace[tempPolyhedralFaces[[adjacent[[j]]]], sharedVertex ];
							Print[StringForm["		sharedVertex: ``", sharedVertex]];
							Print["		neighbors of " <> ToString[sharedVertex] <> " in face " <> ToString[adjacent[[j]]] <> ", " <> ToString[tempPolyhedralFaces[[adjacent[[j]]]]] ];
							Print[neighbors1 ];
							(*get neighbors of the vertex in the current face*)
							neighbors2 = getVertexNeighborsInFace[tempPolyhedralFaces[[f]], sharedVertex ];
							Print["		neighbors of " <> ToString[sharedVertex ] <> " in face " <> ToString[f] <> ", " <> ToString[tempPolyhedralFaces[[f]]] ];
							Print[neighbors2 ];
							(*find duplicates of these vertices now.  We only need to do this for one side.  Choose ancestor face bc fun!!! *)					
							equivalent = {getDuplicateVertices[tempDuplicateVertices, neighbors1[[1]] ], getDuplicateVertices[tempDuplicateVertices, neighbors1[[2]] ]};
							Print[StringForm["		equivalent: ``", equivalent]];
							
							(*find the second vertex in both faces*)
							ancestor2 = -1;
							current2 = -1;
							
							If[MemberQ[equivalent[[1]], neighbors2[[1]] ], (ancestor2 = neighbors1[[1]]; current2 = neighbors2[[1]]; ) ];
							If[MemberQ[equivalent[[1]], neighbors2[[2]] ], (ancestor2 = neighbors1[[1]]; current2 = neighbors2[[1]]; ) ];
							If[MemberQ[equivalent[[2]], neighbors2[[1]] ], (ancestor2 = neighbors1[[2]]; current2 = neighbors2[[1]]; ) ];
							If[MemberQ[equivalent[[2]], neighbors2[[2]] ], (ancestor2 = neighbors1[[2]]; current2 = neighbors2[[2]]; ) ];
							
							Print[StringForm["		duplicate Edges: ``", {{sharedVertex, ancestor2}, {sharedVertex, current2}}]];
							
							(*sometimes there is no second vertex.  TODO: figure out why*)
							If[ancestor2 == -1 || current2 == -1, Continue[]];
							(**)
		
							(*add the edges now*)
							AppendTo[tempDuplicates, {{sharedVertex, ancestor2}, {sharedVertex, current2}}];
							)
						];
						)
					];
				)
				];*)
												
				)
			];
			
			),
			{j, 1, Length[adjacent ]}
		];


		trueAdjacent = findTrueAdjacentFaces[tempPolyhedralFaces, f];		
		(*get unused faces*)
		unused = {};
		Do[
			(
			If[FreeQ[tempAncestors, trueAdjacent[[j]] ], AppendTo[unused, trueAdjacent[[j]] ] ];
			
			),
			{j, 1, Length[trueAdjacent ]}
		];
		
(*		Write[strm, StringForm["indices of unused faces: ``", unused]];*)

		(*update necessary data before starting recursion *)			
		AppendTo[tempFaces, tempPolyhedralFaces[[f]] ]; (*the faces variable is just for debugging purposes or for easily drawing the net*)
		(*tempFaces = ReplacePart[tempFaces, f -> tempPolyhedralFaces[[f]]];*)
		AppendTo[tempAncestors, f];
		tempParent = f;
		
(*		Write[strm, StringForm["tempFaces before recursion: ``", tempFaces]];*)
		
		(*No faces left?  Termination condition *)
		If[ unused == {}, Null,			
			(
			 
			Do[
				(
				(*check to make sure the face is still unused (previous recursive calls may have knocked it out) *)
				If[MemberQ[tempAncestors, unused[[j]] ], Continue[] ];
				(*update list of adjacent faces*)
				AppendTo[tempAdjacentFaces, {f, unused[[j]] } ];
				
				result = recursiveUnfold[tempPolyhedralFaces, tempParent, tempAncestors, tempFaces, tempAdjacentFaces, tempDuplicates, tempDuplicateVertices, unused[[j]], strm ];
				(*update our information for the next recursive call*)
				tempPolyhedralFaces = result[[1]];
				tempAncestors = result[[2]];
				tempFaces = result[[3]];
				tempAdjacentFaces = result[[4]];
				tempDuplicates = result[[5]];
				tempDuplicateVertices = result[[6]];
				
				),
				{j, 1, Length[unused] }
			];
			)
		];
				
		AppendTo[output, tempPolyhedralFaces];
		AppendTo[output, tempAncestors];
		AppendTo[output, tempFaces];
		AppendTo[output, tempAdjacentFaces];
		AppendTo[output, tempDuplicates];
		AppendTo[output, tempDuplicateVertices];

		Return[output];
	];


(*Finds all equivalencies of a specified vertex. Convenience function.
	Parameters:
	duplicates - list of duplicates, in form {{v1, duplicate}, {v2, duplicate}, ...}
	vertex - vertex to find duplicates of
	
	Output:
	{list of equivalent vertices}	
*)
getDuplicateVertices[duplicates_, vertex_]:=
	Module [{},
		Return[recursiveGetDuplicateVertices[duplicates, vertex, {}]];
	];


(*Recursive part of getDuplicateVertices.
	avoid - list of numbers to avoid when making recursive calls (to prevent infinite loops)
*)
recursiveGetDuplicateVertices[duplicates_, vertex_, avoid_] :=
	Module [{output, tempAvoid, i, j, tempOutput, result},
		tempOutput = {};
		tempAvoid = avoid;
		
		Do[
			(
			If[MemberQ[duplicates[[j]], vertex],
				(
				(*make sure the duplicate isn't in the avoid list*)
				If[MemberQ[tempAvoid, duplicates[[j]][[1]] ] || MemberQ[tempAvoid, duplicates[[j]][[2]] ], Continue[] ];
				
				(*make sure the vertex isn't both elements of the list*)
				If[duplicates[[j]][[1]] != duplicates[[j]][[2]],
					(
					(*don't include the vertex we're using*)
					If[duplicates[[j]][[1]] == vertex, AppendTo[tempOutput , duplicates[[j]][[2]]] ];
					If[duplicates[[j]][[2]] == vertex, AppendTo[tempOutput , duplicates[[j]][[1]]] ];
					)
				];
				)
			];
			),
			{j, 1, Length[duplicates]}
		];
		
		(*add current vertex to avoid list*)
		AppendTo[tempAvoid, vertex];

		(*we need a separate list to keep track of total results*)		
		output = tempOutput;
		
		(*find the equivalents of every duplicate*)
		Do[
			(
			result = recursiveGetDuplicateVertices[duplicates, tempOutput[[j]], tempAvoid];
	(*		Print["result"];
			Print[result];
			Print["from"];
			Print[tempOutput[[j]] ];*)
				
			(*add the results to the output*)
			Do[
				(
					AppendTo[output, result[[i]] ];
				),
				{i, 1, Length[result]}
			];		
			),
			{j, 1, Length[tempOutput]}
		];		
		
		
		Return[output];
	];


(*Gets the neigbors of a vertex in a face -- there will always be two.
	Parameters:
	face - vertices which comprise the face {1,2,3,...} -- assumption is that each vertex is adjacent to the next, winding order is irrelevant
	vertex - vertex in face
	
	Output:
	{neighbor1, neighbor2}
*)
getVertexNeighborsInFace[face_, vertex_] :=
	Module[{tempFace, pos, output},
		tempFace = face;
				
		(*find index of vertex*)
		pos = Position[tempFace, vertex][[1]][[1]];
		(*make sure the index isn't at the beginning or end, just to make our life easier*)
		If[pos == 1, tempFace = RotateRight[tempFace, 1]];
		If[pos == Length[tempFace], tempFace = RotateLeft[tempFace, 1] ];

		(*find the index again*)
		pos = Position[tempFace, vertex][[1]][[1]];
		
		(*Put the neighbors in output*)
		output = {tempFace[[pos-1]], tempFace[[pos+1]] };
		Return[output];				 
	];


(*
	Parameters:
	x graph - net of form {V,E}, where V is vertex set (vertices are numbers) and E is edge set
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - dual of input net, where each face is a vertex whose value is its position in the faces set
	duplicates - duplicate edges in graph, with endpoints in clockwise order
	duplicateVertices - list of which vertices are duplicates { {1,2}, {2,3}, {1,4}, ...}
	numVertices - number of vertices in net
	edgeLength - length of each edge
	verbose - number of frames per fold animation.  0 indicates no animation should be created
			
	filePath - can be absolute or relative, must be a string formatted so Mathematica can read it. If relative, Mathematica will assume root is Documents directory (for Windows anyway)
	alignFreq - how often to align first face with xy plane 
				0: never
				-1: at end of folding
				x >= 1: every x folds 
	Output:
	{Graphics3D object (representation of folded net), coordinates of every face (rounded to 2 decimal places) }
*)
findCoordinates[faces_, adjacentFaces_, duplicates_, duplicateVertices_, numVertices_, edgeLength_, filePath_, verbose_, alignFreq_] :=
	Module[ {done, coordinates, d, temp, i,j, axes, usedAxes, joinedPoints, graphicsFaces, graphicsFaces2, edgesFixed, stl, foldNumber, frames, result, bounds, faceCoordinates, num},
		(*Print["hi"];*)
		done = {};
		usedAxes = {};
		joinedPoints = {};
		frames = {};
		(*generate coordinates for vertex set of net*)
		result = createNetCoordinates[faces, adjacentFaces, numVertices, edgeLength];		 
		coordinates = result[[1]];
		bounds = result[[2]];

		(*Print[bounds];*)
		(*Print["hi2"];*)

		(*convert everything to numerical values*)
		coordinates = N[coordinates];

		(*create list of axes for each face -- will result in duplicate axes, facing in opposite directions*)
		axes = createAxes[faces, adjacentFaces];

		(*Show first step of the folding process*)
		(*If[verbose == 1, AppendTo[frames, displayShape[coordinates, faces]], Null];*)
		(*Print[displayShape[coordinates, faces, {}]];*)
		
		edgesFixed = 0;
		num = 0;
		foldNumber = 1;
		While[Length[done] < Length[duplicates],
			num = Length[done];
		
			Do[
				(d = duplicates[[i]];
				(*If the duplicates haven't already been taken care of, then try do it now*)
				If[FreeQ[done, d],
					(
					(*This is the heart of the algorithm*)
					temp = connect[coordinates, faces, adjacentFaces, axes, usedAxes, joinedPoints, d, duplicateVertices, filePath, verbose, foldNumber, {}];
					
					(*Was it successful?  If no, then empty list returned*)
					If[Length[temp] > 0,
						
						(*keep track of changed coordinates and axes list if successful*)
						(coordinates = temp[[1]];
						usedAxes = temp[[2]];
						joinedPoints = temp[[3]];
						(*add duplicates to "done"*)
						AppendTo[done, d];
						
						(*Print[duplicateVertices];
						Print[d];
						Print[displayShape[coordinates, faces, {}]];*)

						(*Show each step of the folding process*)
						If[temp[[4]] == 1, (*The flag at the end indicates when something significant happens to the shape of the net*)
							(
							foldNumber = foldNumber + 1; (*incremember fold number only if animation happened*)
							(*add frames to current list of frames*)
							If[verbose > 0 (*, AppendTo[frames, displayShape[coordinates, faces]]*),
								(*list of frames?*)
								(
								Do[
									(
									AppendTo[frames, temp[[5]][[j]] ];
									),
									{j, 1, Length[temp[[5]] ]}
								];
								)
							];
							
							(*align face 1 with xy plane every 5 folds*)
							If[alignFreq > 0,
								(
								If[Divisible[foldNumber, alignFreq],
									(
									result = alignShape[faces, adjacentFaces, coordinates, verbose];
									coordinates = result[[1]];
									Do[
										(
										AppendTo[frames, result[[2]][[j]] ];
										),
										{j, 1, Length[result[[2]] ]}
									];
									
									)								
								];
								)
							];
							
							
							edgesFixed = 0;
							),
							edgesFixed = 1;
						];
						),
						Null
						]
					),
					
					Null
				];
				),
				 {i, Length[duplicates]}
			];
			
			If[Length[done] == num,
				(
				Print["Error occured during folding: can't find valid edge to join at current stage"];
				Break[];
				)
			];
			
		];
		(*Display one more time if edges were fixed since last display*)
		If[edgesFixed == 1,
			If[verbose >= 1, AppendTo[frames, displayShape[coordinates, faces, {}] ]];
			,Null];		

		If[alignFreq != 0,
			(
			(*align shape with xy plane one last time*)
			result = alignShape[faces, adjacentFaces, coordinates, verbose];
			coordinates = result[[1]];
			Do[
				(
				AppendTo[frames, result[[2]][[j]] ];
				),
				{j, 1, Length[result[[2]] ]}
			];
			)
		];
		

		stl = displayShape[coordinates, faces, {}];
		(*create list of coordinates for each face*)
		faceCoordinates = {};
		Do[
			(
			AppendTo[faceCoordinates, {}];
			Do[
				(
				AppendTo[faceCoordinates[[Length[faceCoordinates] ]], Round[{coordinates[[faces[[j]][[i]] ]]}, .01] ];
				),
				{i, 1, Length[faces[[j]]]}
			];
			),
			{j, 1, Length[faces]}
		];
		
		(*export completed animation*)
		If[verbose > 0, Export[filePath<>".gif", frames]];
		Export[filePath<>".stl", stl];		
		
		Return[{stl, faceCoordinates}];
				
	];
	
	
displayShape[coordinates_, faces_, bounds_]:=
Module[{graphicsFaces, graphicsFaces2, i, j, range, colors, graphic},
	graphicsFaces = {};
	colors = {};
	Do[
		(
		AppendTo[graphicsFaces, {}];
		If[i == 1, AppendTo[colors, Purple], AppendTo[colors, {}] ];
		
		Do[
			(
				AppendTo[graphicsFaces[[i]], coordinates[[faces[[i]][[j]]]] ];
			),
			{j, Length[faces[[i]]]}
		];

		),
		{i, Length[faces]}
	];
	graphicsFaces2 = {};
	Do[
		(
(*		AppendTo[graphicsFaces2, Graphics3D[Polygon[graphicsFaces[[i]] ] ]];*)
		AppendTo[graphicsFaces2, Polygon[graphicsFaces[[i]] ]];
		),
		{i, Length[faces]}
	];
	If[bounds == {},
		(
		range = Automatic;
		),
		(
		range = {{bounds[[1]],bounds[[2]]}, {bounds[[1]],bounds[[2]]}, {bounds[[1]],bounds[[2]]}};
		)
	];
	graphic = Graphics3D[Table[ {colors[[i]], graphicsFaces2[[i]]}, {i, 1, Length[faces] }] ];  

	(*Print[Show[graphicsFaces2]];*)
	(*Return[Show[graphicsFaces2, PlotRange -> range] ]*)
(*	Return[Show[graphicsFaces2] ]*)
	Return[Show[graphic] ]

];


(*Creates the coordinates of a specified net.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - edge set for faces, where each face is a vertex whose value is its position in the faces set
	numVertices - total number of vertices, so that we don't have to calculate this from faces
	edgeLength - length of each edge
	
	Output:
	List of planar 3D points - representation of unfolded net, each point corresponds to the point in the graph (in findCoordinates) occupying the same list location
	bounds -- min and max values found among the coordinates {b1, b2}
*)
createNetCoordinates[faces_, adjacentFaces_, numVertices_, edgeLength_] :=
	Module[{n, s, interiorAngle, exteriorAngle, i, points, previousPoint, currentAngle, vertex, done, j,
		 currentFace, adjFace, endpoint1, endpoint2, t, difference, endindex1, endindex2, tempFaces, count, output, val},
		
		(*Print["faces"];
		Print[faces];
		Print["adjacentFaces"];
		Print[adjacentFaces];
		Print["numVertices"];
		Print[numVertices];*)

		tempFaces = faces; (*Because apparently you can't modify input arguments*)
		
		(*
		According to the Interior Angle Theorem,
		S = (n-2)*pi, where S is the sum of the interior angles of the polygon, and n is the number of edges 
		https://study.com/academy/lesson/interior-angle-theorem-definition-formula.html
		
		So from this we can create each face.
		*)
		
		(*
		Kickstart the algorithm by setting the first point in the first face to (0,0,0), and the next point to (edgeLength, 0, 0)
		So, we are starting the first angle at 0 radians, and decrementing the angle by S/n radians after each edge, working clockwise.
		The next face will be selected so that it is adjacent to the first face.  Once we find the edge it shares with the first face, we will build from that.
		All faces will be generated the same way, building from a previously completed edge.
		*)
		
		(*First, find the interior angle of the first face*)
		n = Length[tempFaces[[1]]];
		s = (n-2) * Pi;
		interiorAngle = s/n;
		(*Now calculate exterior angle because that's more useful to us*)
		exteriorAngle = Pi - interiorAngle;
		
		(*coordinates list, initialized with placeholders*)
		points = Array[f, numVertices];
		Do[points = ReplacePart[points, i-> -1], {i, 1, numVertices} ];
		
		(*set first point to (0,0,0) *)
		points = ReplacePart[points, tempFaces[[1]][[1]] -> {0,0,0} ];
		
		(* previousPoint and currentAngle will be used to figure out the next vertex location *)
		currentAngle = 0;
		previousPoint = {0,0,0};
		Do[
			(vertex = tempFaces[[1]][[i]];
			(*Make sure the vertex is not already set (won't be an issue for the first face) *)
			If[points[[vertex]] == -1,
				(*set the next point and update previousPoint and currentAngle*)
				previousPoint = N[{previousPoint[[1]] + Cos[currentAngle]*edgeLength, previousPoint[[2]] + Sin[currentAngle]*edgeLength, previousPoint[[3]]}];
				currentAngle -= exteriorAngle; (*decrement because we're working clockwise*)
				points = ReplacePart[points, vertex -> previousPoint ]; (*right now, "previous point" is current point*)
				
				, Null (*Don't do anything if the vertex is already set*)
			];
			
			),
			{i, 2, Length[tempFaces[[1]]]} (*start with the second vertex, because the first vertex was already set*)
		];
		
		(*Now for the main part of the algorithm.  Keep looping until every face is accounted for.*)
		(*list of finished faces, initialized with first face on the list*)
		done = {1};
		count = 0; 
		While[Length[done] < Length[tempFaces],
			(*Find a new face to work with*)
			adjFace = -1; (* adjacent face, don't confuse with adjacentFaces which is an edge set *)
			currentFace = -1;
			Do[
				(
				(*Find an edge with a finished face as an endpoint, and assign currentFace to the other endpoint if it's not also a finished face *)
				(*This is just an if..else if block*)
				If[MemberQ[done, adjacentFaces[[i]][[1]] ] && FreeQ[done, adjacentFaces[[i]][[2]] ] , (adjFace = adjacentFaces[[i]][[1]];
					currentFace = adjacentFaces[[i]][[2]]),
					If[MemberQ[done, adjacentFaces[[i]][[2]] ] && FreeQ[done, adjacentFaces[[i]][[1]] ] , (adjFace = adjacentFaces[[i]][[2]];
						currentFace = adjacentFaces[[i]][[1]]), Null]
				];

				),
				{i, 1, Length[adjacentFaces]}
			];
			
			
			
			(*Find the angle of the edge shared by both faces*)
			
			(*First, find the two endpoints shared by both faces*)
			endpoint1 = -1;
			endpoint2 = -1;
			endindex1 = -1;
			endindex2 = -1;
			
			Do[
				(
				If[MemberQ[tempFaces[[currentFace]], tempFaces[[adjFace]][[i]] ], (endpoint1 = points[[tempFaces[[adjFace]][[i]] ]]; endindex1 = tempFaces[[adjFace]][[i]]; Break[]), Null ];
				),
				{i, 1, Length[tempFaces[[adjFace]]]}];
			Do[
				(
				If[MemberQ[tempFaces[[currentFace]], tempFaces[[adjFace]][[j]] ] && endpoint1 != points[[tempFaces[[adjFace]][[j]] ]], (endpoint2 = points[[tempFaces[[adjFace]][[j]] ]]; endindex2 = tempFaces[[adjFace]][[j]]; Break[]), Null ];
				),
				{j, 1, Length[tempFaces[[adjFace]]]}];
			
			
			(*Now calculate the angle between the two, after first making sure the endpoints are in clockwise order.*)
			(*The only time the endpoints are not in clockwise order is if the first and last vertices are shared with the current face*)
			If[endpoint1 == points[[tempFaces[[adjFace]][[1]] ]] && endpoint2 == points[[tempFaces[[adjFace]][[Length[tempFaces[[adjFace]]] ]] ]], ( t = endpoint1; endpoint1 = endpoint2; endpoint2 = t; endindex2 = endindex1), Null];
			difference = endpoint1 - endpoint2;


(*			If[(endpoint1 == -1 || endpoint2 == -1) && count != currentFace,
				(
				count = currentFace;
				Print[StringForm["currentFace: ``", currentFace] ];
				Print[StringForm["adjFace: ``", adjFace] ];
				Print[StringForm["endpoint1: ``", endpoint1] ];
				Print[StringForm["endpoint2: ``", endpoint2] ];
				Print[StringForm["difference: ``", difference] ];
				)
			];*)


			currentAngle = ArcTan[difference[[1]], difference[[2]]]; (*elements in vertices are assumed to be {x,y,z} *)
			
			(*Find the starting point now*)
			previousPoint = endpoint2; (*Start at endpoint2, work back to endpoint1, and then start creating new edges*)
			
			(*Rotate the currentFace vertices so that the first vertex in the list is the same as previousPoint*)
			While[tempFaces[[currentFace]][[1]] != endindex2,
				tempFaces[[currentFace]] = RotateLeft[tempFaces[[currentFace]], 1]
			];
			
			(*Calculate the interior angle of the current face*)
			n = Length[tempFaces[[currentFace]]];
			s = (n-2) * Pi;
			interiorAngle = s/n;
			(*Now calculate exterior angle*)
			exteriorAngle = Pi - interiorAngle;
			
			Do[
				(vertex = tempFaces[[currentFace]][[i]];
				(*update previousPoint and currentAngle*)
				previousPoint = N[{previousPoint[[1]] + Cos[currentAngle]*edgeLength, previousPoint[[2]] + Sin[currentAngle]*edgeLength, previousPoint[[3]]}];
				currentAngle -= exteriorAngle; (*decrement because we're working clockwise*)
				(*Make sure the vertex is not already set *)
				If[points[[vertex]] == -1,
					points = ReplacePart[points, vertex -> previousPoint ]; (*right now, "previous point" is current point*)					
					, Null (*Don't do anything if the vertex is already set*)
				];
				
				),
				{i, 2, Length[tempFaces[[currentFace]]]} (*start with the second vertex, because the first vertex was already set*)
			];
			
			(*We're done with this face now*)
			AppendTo[done, currentFace];
			
			];
				
		val =  Max[points,2] - Min[points,2];
		output = {points, {-val + Min[points,2], val + Max[points,2]} };
		Return[output]
	]


(*Finds folding axes on a net.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - edge set for faces, where each face is a vertex whose value is its position in the faces set
	
	Output:
	list of axes for each face, defined by their endpoints in clockwise order.  Axis element corresponds to edge element in adjacentFaces
*)
createAxes[faces_, adjacentFaces_] :=
	Module[{edges, i, m, n, one, two, temp, result},
		edges = {};

		Do[
			(
			AppendTo[edges, {}];
			Do[
				(
				(*Check to see if this edge is incident to the face*)
				If[adjacentFaces[[i]][[1]] == m || adjacentFaces[[i]][[2]] == m, (
			
				(*Get shared vertices*)
				result = findSharedVertices[faces, adjacentFaces[[i]][[1]], adjacentFaces[[i]][[2]]];
				one = result[[1]];
				two = result[[2]];
				
				(*Arrange the endpoints in clockwise order*)
				Do[
					(
						(*Swap endpoints if two doesn't come after one*)
						If[faces[[m]][[n]] == one,
							(
							If[ n == Length[faces[[m]]], (*Wrap around if one is at the end*)
								(
								If[ faces[[m]][[1]] == two, Null, (temp = one; one = two; two = temp)];
								),
								(
								If[ faces[[m]][[n+1]] == two, Null, (temp = one; one = two; two = temp)];
								)
							];
							 
							),							 
							Null];
					),
					{n, 1, Length[faces[[m]]]}
				];
													
				(*Create our new axis using the endpoints we found*)
				AppendTo[edges[[m]], {one, two}];), Null ];
				),
				{i, 1, Length[adjacentFaces]}
			];
			),
			{m, 1, Length[faces]}
		];
		Return[edges]
	]


(*
	This function finds the pair of axes needed to rotate two duplicate edges together.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - edge set for faces, where each face is a vertex whose value is its position in the faces set
	axes - list of axes for each face, defined by their endpoints in clockwise order
	usedAxes - list of previously rotated axes, contains duplicate but flipped axes to simplify searches
	duplicatePair - pair of duplicate edges in graph
	
	Output:
	pair of axes to rotate faces around, along with faces that will be rotated by each axis.  However, if there is a previously unused (ie not rotated) axis
	lying on the path from the face of one duplicate edge to the face of the other duplicate edge, then an empty list {} will be returned to signify that the
	edges are not ready to be brought together yet.  Why?  Because if, at this point, the axes are rotated and the edges are brought together, then there will be at least two faces
	that will remain coplanar, and the unused axis (or axes) cannot be rotated in the future or the newly-connected edges would no longer connect.
*)
findAxes[faces_, adjacentFaces_, axes_, usedAxes_, duplicatePair_] :=
	Module[{info, d1, d2, i, j, k, path, firstAxis, secondAxis, pair, firstFaces, secondFaces, modifiedEdges, index, tempPath, num, numUnusedAxes},
		
		(*Find which two faces the duplicate edges belong to*)
		d1 = -1;
		d2 = -1;
		Do[ 
			( 
				Do[
					(
						num = 0; (*number of vertices from duplicatePair found in the current face -- must be two for the axis  *)
						Do[
							(
								(*Are the vertices the same?*)
								If[faces[[j]][[k]] == duplicatePair[[i]][[1]] || faces[[j]][[k]] == duplicatePair[[i]][[2]],
									(*We found which face the duplicate vertex belongs to*)
									(num = num + 1; (* increment counter *)									
									If[num >= 2, If[d1 == -1, d1 = j, d2 = j], Null ]), (*we found a valid face if it contains the edge defined by the duplicate *)
									Null
								];
							),
							{k, 1, Length[faces[[j]]]}
						];
						(*Stop searching through faces if the face is already found; optimization tweak that may be significant for large nets*)
						If[d1 != -1 && i == 1, Break[], Null];
						If[d2 != -1 && i == 2, Break[], Null];
					),
					{j, 1, Length[faces]}
				];
			 ), 
			 {i, 1, Length[duplicatePair]} (*if duplicatePair has more than 2 elements then there will be problems...  could hardcode length as 2 but would reduce readability*)
		 ]; 
		
		
		(*Starting from the first duplicate edges's face, traverse to the second face*)
		path = findPath[d1, d2, Length[faces], adjacentFaces];
				
		(*Check for any unused axes along the path (other than the start and the end).  If there are any, the duplicate edges can't be brought together yet, so return an empty list. *)
		numUnusedAxes = findAxesOnDualPath[faces, axes, usedAxes, path];
		
		If[numUnusedAxes > 0, info = {}, 
			(
		
			(*The only edges we're interested in are the last two, because these are the axes of rotation*)
			(*find axis corresponding to the first edge*)
			firstAxis = {};
			secondAxis = {};
			
			(*get the vertices that make up the first axis*)
			pair = findSharedVertices[faces, path[[2]][[1]], path[[2]][[2]]];
			Do[
				(
					(*Is this the right axis?*)
					If[MemberQ[axes[[d1]][[i]], pair[[1]]] && MemberQ[axes[[d1]][[i]], pair[[2]]], (firstAxis = {d1, i}; Break[]) ,Null ];
				),
				{i, 1, Length[axes[[d1]]]}
			];
	
			(*get the vertices that make up the second axis*)
			pair = findSharedVertices[faces, path[[Length[path]-1]][[1]], path[[Length[path]-1]][[2]]];
			Do[
				(
					(*Is this the right axis?*)
					If[MemberQ[axes[[d2]][[i]], pair[[1]]] && MemberQ[axes[[d2]][[i]], pair[[2]]], (secondAxis = {d2, i}; Break[]) ,Null ];
				),
				{i, 1, Length[axes[[d2]]]}
			];
			
			
			(*Find the faces that will be rotated by the first axis*)
			firstFaces = {d1};
			secondFaces = {d2};
			modifiedEdges = adjacentFaces;
			index = -1;
			(*From the dual graph, remove the edge connecting the first face to the rest of the graph*)
			Do[
				(
					If[adjacentFaces[[i]] == path[[2]], (index = i; Break[]), Null];
				),
				{i, 1, Length[adjacentFaces]}
			];
			modifiedEdges = Delete[modifiedEdges, index];
			
			(*Now see which faces are still connected to the first face*)
			Do[
				(
					If[d1 != i,
						(tempPath = findPath[d1, i, Length[faces], modifiedEdges];
						If[tempPath != {}, AppendTo[firstFaces, i], Null];
						), Null];
				),
				{i, 1, Length[faces]}
			];	
	
			(*Repeat these steps for the second axis*)
	
			(*From the dual graph, remove the edge connecting the second face to the rest of the graph*)
			modifiedEdges = adjacentFaces;
			Do[
				(
					If[adjacentFaces[[i]] == path[[Length[path]-1]], (index = i; Break[]), Null];
				),
				{i, 1, Length[adjacentFaces]}
			];
			modifiedEdges = Delete[modifiedEdges, index];
	
			(*Now see which faces are still connected to the first face*)
			Do[
				(
					If[d2 != i,
						(tempPath = findPath[d2, i, Length[faces], modifiedEdges];
						If[tempPath != {}, AppendTo[secondFaces, i], Null];
						), Null];
				),
				{i, 1, Length[faces]}
			];
		
			info = {{firstAxis, firstFaces}, {secondAxis, secondFaces}};
			)	
		];

				
		Return[info]
	]


(*Finds number of unused axes on the path of the net graph dual, minus the first and last axes which are irrelevant for the reasons behind this calculation.

*)
findAxesOnDualPath[faces_, axes_, usedAxes_, path_] :=
	Module [ {pair, traversalLength, result},
		
		traversalLength = Floor[Length[path]/2];		
		result = 0; (*initialize return result to 0*)
		
		If[traversalLength <= 2, Null, (*If the length of the path <= 2 (should never be less than 2 though), then the only axes are the first and last, which are ignored anyway*)
			(
		
			(*get the vertices that make up each axis, other than the first and last axes*)
			Do[
				(			
				(*find which vertices are shared by the current two faces*)
				pair = findSharedVertices[faces, path[[2*j]][[1]], path[[2*j]][[2]]];				
				Do[
					(
						(*Is this the right axis?*)
						If[MemberQ[axes[[path[[2*j-1]] ]][[i]], pair[[1]]] && MemberQ[axes[[path[[2*j-1]] ]][[i]], pair[[2]]], 
							(
							(*Is this axis already used?  If not, add to count*)
							If[MemberQ[usedAxes, axes[[path[[2*j-1]] ]][[i]] ], Null, result = result + 1  ]; Break[]
							) ,Null ];
					),
					{i, 1, Length[axes[[path[[2*j-1]] ]]]} (*Could also be path[[2*j+1]]; the used axis list contains both duplicate axes so irrelevant*)
				];
				),
				{j, 2, traversalLength-1} (*iterate over the traversal length of the path, ignoring first and last edges (which correspond to first and last axes) *)
			];
			)
		];
		
		(*Return number of unused axes found on path -- returning true/false would be faster and just as informative*)
		Return[result]
	]


(*Find faces which share an edge with a specified face.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, though winding order doesn't matter
	f - index of specified face in faces list
	
	Output:
	{index list of adjacent faces} 
*)
findTrueAdjacentFaces[faces_, f_] :=
	Module[{i, vertices, adjacent},
		
		(*output*)
		adjacent = {};
		Do[
			(
				(*Do the two faces share two vertices?*)
				vertices = findSharedVertices[faces, i, f];
				If[(vertices[[1]] != -1 && vertices[[2]] != -1) && i != f,
					AppendTo[adjacent, i]; (*add the index of the adjacent face to the list*)
				];
			),
			{i, 1, Length[faces]}
		];
		
		Return[adjacent]
	];


(*Finds the faces adjacent to a specified face.  "Adjacent" meaning sharing at least one vertex -- not the true definition of adjacent faces!
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, though winding order doesn't matter
	f - index of specified face in faces list
	
	Output:
	{index list of adjacent faces} 
*)	
findAdjacentFaces[faces_, f_]:=
	Module[{i, vertices, adjacent},
				
		adjacent = {}; (*output*)
		Do[
			(
				(*Do the two faces share any vertices?*)
				vertices = findSharedVertices[faces, i, f];
				If[(vertices[[1]] != -1 || vertices[[2]] != -1) && i != f,
					AppendTo[adjacent, i]; (*add the index of the adjacent face to the list*)
				];
			),
			{i, 1, Length[faces]}
		];
		
		Return[adjacent]
	];


(*Finds the vertices shared by two faces in a net.  Will also work to see if two faces in any graph are adjacent, but will only give two shared vertices
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order! 
	face1 - index of first face
	face2 - index of second face (order is irrelevant)
	
	Output:
	Pair of shared vertices, in random order
*)
findSharedVertices[faces_, face1_, face2_] :=
	Module [{one, two, j, k},
		
		(*reset endpoints*)
		one = -1;
		two = -1;
		(*
		Take the first face and iterate through every vertex.  For each vertex, iterate through every vertex in the second face and try to find a match.
		If it exists, assign it as one of the shared vertices.
		*)
		Do[
			(
				Do[
					(
						(*Are the vertices the same?*)
						If[faces[[face1]][[j]] == faces[[face2]][[k]],
							(*
							Since each face shares at most one edge with every other face, and each edge has two endpoints,
							we don't have to worry about breaking the loop after the second endpoint is found 
							*)
							If[one == -1, one = faces[[face1]][[j]], two = faces[[face1]][[j]]],
							Null
						];
					),
					{k, 1, Length[faces[[face2]] ]}
				];
		
			),
			{j, 1, Length[faces[[face1]] ]}
		];
		
		Return[{one, two}]				
	]


(*
	Parameters:
	start - start vertex
	end - end vertex, can be the same as the start vertex! (ie we're looking for a cycle)
	numVertices - length of vertex set of graph
	edges - edge set of graph
	
	Output:
	path from start to end, in form { vertex, edge, vertex, edge, ..., vertex, edge, vertex}, where the vertices on either side of each edge are its endpoints, and each edge
	{} if no path exists
*)
findPath[start_, end_, numVertices_, edges_] :=
	Module[{incidentEdges, traversal, i, j, result},

		(*First, create list of edges which are incident to each vertex.  This will make traversing the graph much easier.*)
		(*incidentEdges will look like { { edge, edge, ...}, {...}, ... }, with each element edge incident to the vertex corresponding to the element numer*)
		incidentEdges = {};
		
		(*Go through every vertex and find every edge incident to it*)
		Do[
			AppendTo[incidentEdges, {}];
			(
				Do[
					(
						(*Is vertex i an endpoint of edge j?  If so, add edge to incidentEdges*)
						(*If[ MemberQ[edges[[j]], i], If[Length[incidentEdges] < i, AppendTo[incidentEdges, {edges[[j]]}], AppendTo[incidentEdges[[i]], edges[[j]]]], Null];*)
						If[ MemberQ[edges[[j]], i], AppendTo[incidentEdges[[i]], edges[[j]]], Null];
					),
					{j, 1, Length[edges]}
				];
			),
			{i, 1, numVertices}
		];
		
		(*call recursive helper function now*)
		traversal = recurseTraverse[{start}, start, end, incidentEdges]; 
		
		result = {};
		If[traversal[[2]] == True, result =  traversal[[1]],Null];
		Return[result]
	]


(*Recursive helper function, not intended to be directly called (can be done easily enough though).  Call findPath instead.
	Parameters:
	traversal - current traversal, should be initialized with starting vertex
	vertex - current vertex to branch from
	endVertex - desired end vertex
	incidentEdges - list of edges which are incident to each vertex
	
	Output:
	list with a path and True if path ends at wanted vertex, False otherwise
	Note: ultimate output wil be the path that leads to the wanted vertex, if that path exists 	
*)
 recurseTraverse[traversal_, vertex_, endVertex_, incidentEdges_] :=
 	Module[{tempTraversal, openings, newVertex, result},
			 		
 		tempTraversal = traversal; (*Because input arguments can't be modified*)
 		
 		(*Find acceptable edges*)
 		openings = {}; 		
 		Do[
 			(
 				(*Is this edge and at least one endpoint not previously used?  If one endpoint is the end vertex then accept it, this way we can find cycles*)
 				(*|| endVertex == incidentEdges[[vertex]][[i]][[1]] || endVertex == incidentEdges[[vertex]][[i]][[2]]*)
 				If[FreeQ[tempTraversal, incidentEdges[[vertex]][[i]]] && (FreeQ[tempTraversal, incidentEdges[[vertex]][[i]][[1]]] || FreeQ[tempTraversal, incidentEdges[[vertex]][[i]][[2]]]) , AppendTo[openings, incidentEdges[[vertex]][[i]]], Null];
 			),
 			{i, 1, Length[incidentEdges[[vertex]]]}
 		];
 		
 		(*Termination condition when dead end reached*)
 		If[Length[openings] ==  0, (result = {tempTraversal, False}),  (*False because this path does not end with the wanted vertex*)
 		
	 		(*Iterate over acceptable edges*)
	 		Do[
	 			(
	 				newVertex = -1;
	 				(*Find which endpoint hasn't been traversed to yet*)
	 				If[FreeQ[tempTraversal, openings[[i]][[1]]], newVertex = openings[[i]][[1]], newVertex = openings[[i]][[2]]];
	 				AppendTo[tempTraversal, openings[[i]]];
	 				AppendTo[tempTraversal, newVertex];
	 				
	 				(*Happy termination condition*)
	 				(*Have we reached the wanted vertex?  If so, initiate exit*)
	 				If[newVertex == endVertex, (result = {tempTraversal, True}; Break[]), Null];
	 				
	 				(*Now for the recursive part*)
	 				result = recurseTraverse[tempTraversal, newVertex, endVertex, incidentEdges];		
	 				If[result[[2]] == True, (Break[]), Null];
	 				
	 				tempTraversal = Drop[tempTraversal, -2]; (*Begin again on a clean slate*)
	 			),
	 			{i, 1, Length[openings]}
	 			
	 		];
 		];
 		
 		Return[result]; 		 		
 	]


(*This function will attempt to connect two edges, rotating the faces if successful.
	Parameters:
	coordinates - coordinates for vertex set of graph
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	adjacentFaces - edge set for faces, where each face is a vertex whose value is its position in the faces set
	axes - list of axes for each face, defined by their endpoints in clockwise order
	usedAxes - list of previously rotated axes, contains duplicate but flipped axes to simplify searches
	joinedPoints - List of point pairs that were previously joined; order is irrelevant 
	duplicateVertices - list of which vertices are duplicates { {1,2}, {2,3}, {1,4}, ...}
	duplicatePair - pair of duplicate edges in graph, with endpoints in clockwise order
	.
	.
	.
	bounds - boundary values for Show[]	

	Output:
	{{modified coordinates if successful}, {list of used axes (1 or 2)}, {joined points}, 1 if a fold occurs (0 otherwise), {list of animation frames}}.  Empty list {} if unsuccessful.
*)
connect[coordinates_, faces_, adjacentFaces_, axes_, usedAxes_, joinedPoints_, duplicatePair_, duplicateVertices_, filePath_, verbose_, foldNumber_, bounds_] :=
	Module[{data, tempCoordinates, result, t, t2, firstAxis, secondAxis, firstAnchor, secondAnchor, r1, r2, eq1, eq2, as, angle1, angle2, firstPoints, secondPoints, t3, t4, point1, point2, point3, point4
		, tempPoint, vec1, vec2, v, p1, p2, p3, p4, d1, d2, d3, d4, length1, length2, solutions, psolutions, temp1, temp2, i, usedAxesCopy, joinedPointsCopy, firstAxisValid, secondAxisValid, zip, ineqToIntv,
		diff, dist, change, duplicate1, duplicate2, fakeR1, fakeR2, fakeCoordinates, j, string, frames, trueFalse},
		
					
		(*	Print[duplicatePair];*)
		
		tempCoordinates = coordinates; (*Because input arguments can't be modified*)
		
		(*Get the axes we'll be rotating, along with the affected faces*)
		data = findAxes[faces, adjacentFaces, axes, usedAxes, duplicatePair];
		
		as = Length[data];
		
		result = {};
		(*Don't do anything if data returns empty list -- indication that the edges shouldn't be rotated yet*)
		If[as == 0, Null , 
			(
			(*calculate the axes' vectors*)
			t = data[[1]][[1]][[1]];
			t2 = data[[1]][[1]][[2]];
			firstAnchor = tempCoordinates[[axes[[t]][[t2]][[1]]]];
			firstAxis = tempCoordinates[[axes[[t]][[t2]][[2]]]] - firstAnchor;
																																																
			t3 = data[[2]][[1]][[1]];
			t4 = data[[2]][[1]][[2]];
			secondAnchor = tempCoordinates[[axes[[t3]][[t4]][[1]]]];
			secondAxis = tempCoordinates[[axes[[t3]][[t4]][[2]]]] - secondAnchor;
			
			(*Start by assuming both axes are unused*)
			firstAxisValid = 1;
			secondAxisValid = 1;
			(*Get the transformation matrices.  Only rotate an axis if it isn't on the list of used axes.*)
			(*Transformation for first axis*)		
			If[FreeQ[usedAxes, axes[[t]][[t2]]], 
				(r1 = RotationTransform[-a, firstAxis, firstAnchor] (*Why -a instead of a?  How do we know to always rotate by a negative angle? Conjecture (TODO: explain this at some point) *)			
				),
				(r1 = RotationTransform[0, firstAxis, firstAnchor];
				firstAxisValid = 0;
				)
			];
			
			(*Transformation for second axis*)
			If[FreeQ[usedAxes, axes[[t3]][[t4]]], 
				(r2 = RotationTransform[-a2, secondAxis, secondAnchor]			
				),
				(r2 = RotationTransform[0, secondAxis, secondAnchor];
				secondAxisValid = 0;
				)
			];
			
			(*---------------------------------------------------------------------------------------*)
			(*Have these edges inadvertently been joined already? *)
			If[firstAxisValid == 0 && secondAxisValid == 0, (

				(*align the edge endpoints now*)
				(*If[firstAxisValid == 1,
					(
					tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[1]][[1]] -> tempCoordinates[[duplicatePair[[2]][[2]] ]] ];
					tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[1]][[2]] -> tempCoordinates[[duplicatePair[[2]][[1]] ]] ];
					),
					(
					tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[2]][[1]] -> tempCoordinates[[duplicatePair[[1]][[2]] ]] ];
					tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[2]][[2]] -> tempCoordinates[[duplicatePair[[1]][[1]] ]] ];
					)
				];				*)
				
				change = 0;
				
				If[tempCoordinates[[duplicatePair[[2]][[2]] ]] != tempCoordinates[[duplicatePair[[1]][[1]] ]] || tempCoordinates[[duplicatePair[[2]][[1]] ]] != tempCoordinates[[duplicatePair[[1]][[2]] ]],
					(
				(*	change = 1;*)
					tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[1]][[1]] -> tempCoordinates[[duplicatePair[[2]][[2]] ]] ];
					tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[1]][[2]] -> tempCoordinates[[duplicatePair[[2]][[1]] ]] ];
					), Null

				];
				

				result = {tempCoordinates};
				
				(*
				Add the axes to the list of used axes.  The joined/used duplicate edges will be updated outside of this function.
				*)
				usedAxesCopy = usedAxes;
				AppendTo[usedAxesCopy , axes[[t]][[t2]]];
				AppendTo[usedAxesCopy , axes[[t3]][[t4]]];
				AppendTo[usedAxesCopy , {axes[[t]][[t2]][[2]], axes[[t]][[t2]][[1]]}];
				AppendTo[usedAxesCopy , {axes[[t3]][[t4]][[2]], axes[[t3]][[t4]][[1]]}];
	
				(*Add the modified used axes list to the result*)
				AppendTo[result, usedAxesCopy];
				
				(*Modify joined points list*)
				joinedPointsCopy = joinedPoints;
				AppendTo[joinedPointsCopy, {duplicatePair[[1]][[1]], duplicatePair[[2]][[2]]}];
				AppendTo[joinedPointsCopy, {duplicatePair[[1]][[2]], duplicatePair[[2]][[1]]}];
				AppendTo[result, joinedPointsCopy];
				AppendTo[result, change]; (*No fold actually occurred, but update graphics if edge length changed*)
				AppendTo[result, {}]; (*No frames to add*)
				),
			(*---------------------------------------------------------------------------------------*)
				
				
				(

				
				
			(*Create two vectors, which we will use in our system of equations*)
			(*The first vector will be the first duplicate edge, pointing away from the axis of rotation or a previously joined point in that edge*)
				point1 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; (*initialize first and second points in first duplicate edge*)
				point2 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; 			
				p1 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
				p2 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
				d1 = duplicatePair[[1]][[2]]; (*these two variables are for debugging purposes only*)
				d2 = duplicatePair[[1]][[1]];
				zip = 0; (*are the edges being "zipped" together, or are they being brought together like a lid closing?*)
				(*Swap order if the second endpoint is a previously joined point*)
				(*If any one of the endpoints in the first edge is previously joined, then that edge will be "zipped" together with the second edge
					TODO: Note that this conjecture may well be wrong!  Needs further testing
				*)
				Do[
					(
					If[MemberQ[joinedPoints[[i]] , duplicatePair[[1]][[1]]],
						(
						point1 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]];
						point2 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; 			
						p1 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
						p2 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
						d1 = duplicatePair[[1]][[1]];
						d2 = duplicatePair[[1]][[2]];
						zip = 1; (*zip*)
						)
						,
						If[MemberQ[joinedPoints[[i]] , duplicatePair[[1]][[2]]],
							(
							point1 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]];
							point2 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; 			
							p1 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
							p2 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
							d1 = duplicatePair[[1]][[2]];
							d2 = duplicatePair[[1]][[1]];
							zip = 1; (*zip*)
							)
						];
					];
					),
					{i, 1, Length[joinedPoints]}
				];
				
				
				(*If any one of the endpoints in the first edge is on its axis of rotation, then that edge will be "zipped" together with the second edge
					TODO: This conjecture may be wrong!
				*)
				(*NOTE: order of points flipped so that new algorithm would work*)
				If[firstAxisValid == 1,
					(
					If[duplicatePair[[1]][[1]] == axes[[t]][[t2]][[1]] || duplicatePair[[1]][[1]] == axes[[t]][[t2]][[2]],
						(point1 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; (*first point is the point in the first duplicate edge that is also the axis endpoint*)
						point2 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; (*second point in first duplicate edge*)			
						p1 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
						p2 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
						d1 = duplicatePair[[1]][[1]];
						d2 = duplicatePair[[1]][[2]];
						zip = 1;
						
		(*				If[MemberQ[duplicatePair, {6,1}],
							(
							
							(*Print[StringForm["angle2: ``", angle2 ]];*)
							)
						];*)
						(*Print["1.1"];*)
						
						),					
						If[duplicatePair[[1]][[2]] == axes[[t]][[t2]][[1]] || duplicatePair[[1]][[2]] == axes[[t]][[t2]][[2]],
							(point1 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; (*first point is the point in the first duplicate edge that is also the axis endpoint*)
							point2 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; (*second point in first duplicate edge*)			
							p1 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
							p2 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
							d1 = duplicatePair[[1]][[2]];
							d2 = duplicatePair[[1]][[1]];
							zip = 1;
	
					(*		If[MemberQ[duplicatePair, {6,1}],
								(
								
								(*Print[StringForm["angle2: ``", angle2 ]];*)
								)
							];*)
	
							(*Print["1.2"];*)
							)					
							,Null
						];
						
					
						(*(
						If[duplicatePair[[1]][[2]] == axes[[t]][[t2]][[1]] || duplicatePair[[1]][[2]] == axes[[t]][[t2]][[2]],
							(
							point1 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]];
							point2 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; 			
							p1 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
							p2 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
							),
							(
							(*irrelevant tbh*)
							point1 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]];
							point2 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; 			
							p1 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
							p2 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
							)
						];
						)*)
					];
					)					
				];


				If[secondAxisValid == 1,
					(
					If[duplicatePair[[1]][[1]] == axes[[t3]][[t4]][[1]] || duplicatePair[[1]][[1]] == axes[[t3]][[t4]][[2]],
						(point1 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; (*first point is the point in the first duplicate edge that is also the axis endpoint*)
						point2 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; (*second point in first duplicate edge*)			
						p1 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
						p2 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
						d1 = duplicatePair[[1]][[1]];
						d2 = duplicatePair[[1]][[2]];
						zip = 1;
						
					(*	If[MemberQ[duplicatePair, {6,1}],
							(
							
							(*Print[StringForm["angle2: ``", angle2 ]];*)
							)
						];*)
					(*	Print["2.1"];*)
						
						),					
						If[duplicatePair[[1]][[2]] == axes[[t3]][[t4]][[1]] || duplicatePair[[1]][[2]] == axes[[t3]][[t4]][[2]],
							(point1 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; (*first point is the point in the first duplicate edge that is also the axis endpoint*)
							point2 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; (*second point in first duplicate edge*)			
							p1 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
							p2 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
							d1 = duplicatePair[[1]][[2]];
							d2 = duplicatePair[[1]][[1]];
							zip = 1;
							
					(*		If[MemberQ[duplicatePair, {6,1}],
								(
								
								(*Print[StringForm["angle2: ``", angle2 ]];*)
								)
							];*)
						(*	Print["2.2"];*)
							
							)					
							,Null
						];
						
					];				
					)
				];
				

				
				
				(*vec1 is the first duplicate edge, and is one of the vectors we will be using in our system of equations*)
				vec1 = point2 - point1;
				(*get the length of the first vector (should be a number) *)
				(* length1 = Sqrt[vec1[[1]]^2 + vec1[[2]]^2 + vec1[[3]]^2]; *)
				v = p2-p1;
				length1 = Sqrt[v[[1]]^2 + v[[2]]^2 + v[[3]]^2]; (*TODO: replace with Norm[], which does the same thing*)
	
		
						(*odd edge case that pops up occasionally -- first found with square cupola*)
				trueFalse = True;
				If[(FreeQ[getDuplicateVertices[duplicateVertices, d2], axes[[t3]][[t4]][[1]]] && FreeQ[getDuplicateVertices[duplicateVertices, d2], axes[[t3]][[t4]][[2]]]),
					Null,
					(
(*					Print["hi"];*)
					If[firstAxisValid == 1 && secondAxisValid == 1,
						trueFalse = False;
					];
					)
				];
		(*		Print[trueFalse];*)
				
				If[trueFalse,
					
					(			
					(*For the third point, make sure it isn't on the axis of rotation -- this ensures that the second angle of rotation is being represented properly,
					 and also guarantees that the third point isn't either of the previous two points*)
					point3 = {}; (*initialize outside of the if statement for scope reasons, if Mathematica cares about that sort of thing...*)
					(* point4 = {}; 		
					If[duplicatePair[[2]][[1]] == duplicatePair[[1]][[1]] || duplicatePair[[2]][[1]] == duplicatePair[[1]][[2]],
						(point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]
						(* point4 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]] *)
						),
						(point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]
						(* point4 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]] *)
						)
					];*)
					(*
					If[duplicatePair[[2]][[1]] == axes[[t3]][[t4]][[1]] || duplicatePair[[2]][[1]] == axes[[t3]][[t4]][[2]],
						(					
						point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]
						(* point4 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]] *)
						),
						(
						If[duplicatePair[[2]][[2]] == axes[[t3]][[t4]][[1]] || duplicatePair[[2]][[2]] == axes[[t3]][[t4]][[2]],
							(
							point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]
							(* point4 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]] *)
							),
							(
							*)
							(*edge 2 is not connected directly to the second axis*)
								
								(*find point 4 (points for the second vector are out of order for legacy reasons) *)							
								point4 = point1; (*Trust that point1 was chosen to be colinear to the second edge OR
												that edge 1 has no endpoints that are previously joined or on an axis*)
								tempPoint = p1; (*TODO: clean up, tempPoint isn't being used right now*)
								d4 = d1;
								(*Is the first point in the first edge colinear to the second edge?  In that case, choose it to be point 4 *)
								(*
								If[N[Dot[v,tempCoordinates[[duplicatePair[[2]][[1]] ]] - p1 ]] == 0 && N[Dot[v,tempCoordinates[[duplicatePair[[2]][[2]] ]]  - p1]] == 0,
									(
									point4 = point1;
									tempPoint = p1
									),
									(
									point4 = point2;
									tempPoint = p2
									)
								];
								*)
								
								(*find point 3 -- choose whatever point on the second edge isn't being used as point 4
									TODO: verify that this is gives us the vector we actually want *)
								If[duplicatePair[[2]][[1]] != d4,
									(
									point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]];
									d3 = duplicatePair[[2]][[1]]
									),
									(
									point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]];
									d3 = duplicatePair[[2]][[2]]
									)
								];
	
								(*
								If[N[Dot[v,tempCoordinates[[duplicatePair[[2]][[1]] ]] - tempPoint ]] > N[Dot[v,tempCoordinates[[duplicatePair[[2]][[2]] ]]  - tempPoint]],
									(
									point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]];
									d3 = duplicatePair[[2]][[1]]
									),
									(
									point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]];
									d3 = duplicatePair[[2]][[2]]
									)
								];
								*)
	(*						)
						]
						)
					];
					*)
			
	
	
	
	(* ********************************************************************** *)
				(*The first vector will be the first duplicate edge, pointing away from the axis of rotation or a previously joined point in that edge*)
					point4 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]; (*initialize first and second points in first duplicate edge*)
					point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]; 			
					p4 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
					p3 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
					d4 = duplicatePair[[2]][[2]]; (*these two variables are for debugging purposes only*)
					d3 = duplicatePair[[2]][[1]];
					zip = 0; (*are the edges being "zipped" together, or are they being brought together like a lid closing?*)
					(*Swap order if the second endpoint is a previously joined point*)
					(*If any one of the endpoints in the first edge is previously joined, then that edge will be "zipped" together with the second edge
						TODO: Note that this conjecture may well be wrong!  Needs further testing
					*)
					Do[
						(
						If[MemberQ[joinedPoints[[i]] , duplicatePair[[2]][[1]]],
							(
							point4 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]];
							point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]; 			
							p4 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
							p3 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
							
							d4 = duplicatePair[[2]][[1]];
							d3 = duplicatePair[[2]][[2]];
							
						(*	Print["d3"];
							Print[d3];*)
							zip = 1; (*zip*)
							)
							,
							If[MemberQ[joinedPoints[[i]] , duplicatePair[[2]][[2]]],
								(
								point4 = r1[tempCoordinates[[duplicatePair[[2]][[2]] ]]];
								point3 = r1[tempCoordinates[[duplicatePair[[2]][[1]] ]]]; 			
								p4 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
								p3 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
								
								d4 = duplicatePair[[2]][[2]];
								d3 = duplicatePair[[2]][[1]];
								
								zip = 1; (*zip*)
								)
							];
							
						];
						),
						{i, 1, Length[joinedPoints]}
					];
					
					
	
					
					(*If any one of the endpoints in the first edge is on its axis of rotation, then that edge will be "zipped" together with the second edge
						TODO: This conjecture may be wrong!
					*)
					If[duplicatePair[[2]][[1]] == axes[[t3]][[t4]][[1]] || duplicatePair[[2]][[1]] == axes[[t3]][[t4]][[2]],
						(point4 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]; (*first point is the point in the second duplicate edge that is not the axis endpoint*)
						point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]; (*second point in first duplicate edge*)			
						p4 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
						p3 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
	
						
						d4 = duplicatePair[[2]][[1]];
						d3 = duplicatePair[[2]][[2]];
						zip = 1;
						),					
						If[duplicatePair[[2]][[2]] == axes[[t3]][[t4]][[1]] || duplicatePair[[2]][[2]] == axes[[t3]][[t4]][[2]],
							(point4 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]; (*first point is the point in the second duplicate edge that is not the axis endpoint*)
							point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]; (*second point in first duplicate edge*)			
							p4 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
							p3 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
	
							(*If[MemberQ[duplicatePair, {8,7}],
								(
								Print["2"];
								(*Print[StringForm["point2: ``", p2 ]];*)
								)
							];*)
	
							d4 = duplicatePair[[2]][[2]];
							d3 = duplicatePair[[2]][[1]];
							zip = 1;
							)
							,
							(*check to make sure point 3 isn't on other axis either -- will be the duplicate vertex though, so find that first*)
							(
		(*					Print["joinedPoints"];
							Print[joinedPoints];*)
							duplicate1 = -1;
							duplicate2 = -1;
							Do[
								(
	
								If[MemberQ[joinedPoints[[i]], duplicatePair[[2]][[1]] ] ,
									(
									If[duplicatePair[[2]][[1]] == joinedPoints[[i]][[1]] ,
										(duplicate1 = joinedPoints[[i]][[2]]
										),
										duplicate1 = joinedPoints[[i]][[1]]
									];
									), Null
								];
	
								If[MemberQ[joinedPoints[[i]], duplicatePair[[2]][[2]] ] ,
									(
						(*			Print["found:"];
									Print[i];*)
									If[duplicatePair[[2]][[2]] == joinedPoints[[i]][[1]] ,
										(duplicate2 = joinedPoints[[i]][[2]]
										),
										duplicate2 = joinedPoints[[i]][[1]]
									];
									), Null
								];
									
								),
								{i, 1, Length[joinedPoints]}
						 	];
						(* 	Print["first"];
						 	Print[duplicatePair[[2]][[1]]];
						 	Print["duplicate1"];
						 	Print[duplicate1];
						 	Print["second"];
						 	Print[duplicatePair[[2]][[2]]];
						 	Print["duplicate2"];
						 	Print[duplicate2];*)
						 	
							If[duplicate1 == axes[[t]][[t2]][[1]] || duplicate1 == axes[[t]][[t2]][[2]],
								(point4 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]; (*first point is the point in the second duplicate edge that is not the axis endpoint*)
								point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]; (*second point in first duplicate edge*)			
								p4 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
								p3 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
			
								
								d4 = duplicatePair[[2]][[1]];
								d3 = duplicatePair[[2]][[2]];
								zip = 1;
								),					
								If[duplicate2 == axes[[t]][[t2]][[1]] || duplicate2 == axes[[t]][[t2]][[2]],
									(point4 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]; (*first point is the point in the second duplicate edge that is not the axis endpoint*)
									point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]; (*second point in first duplicate edge*)			
									p4 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
									p3 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
			
							
									(*If[MemberQ[duplicatePair, {8,7}],
										(
										Print["3"];
										(*Print[StringForm["point2: ``", p2 ]];*)
										)
									];*)
					
									d4 = duplicatePair[[2]][[2]];
									d3 = duplicatePair[[2]][[1]];
									zip = 1;
									)					
									,Null
								];
							];
							)
						];
						(*(
						If[duplicatePair[[1]][[2]] == axes[[t]][[t2]][[1]] || duplicatePair[[1]][[2]] == axes[[t]][[t2]][[2]],
							(
							point1 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]];
							point2 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]]; 			
							p1 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
							p2 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
							),
							(
							(*irrelevant tbh*)
							point1 = r1[tempCoordinates[[duplicatePair[[1]][[1]] ]]];
							point2 = r1[tempCoordinates[[duplicatePair[[1]][[2]] ]]]; 			
							p1 = tempCoordinates[[duplicatePair[[1]][[1]] ]];
							p2 = tempCoordinates[[duplicatePair[[1]][[2]] ]];
							)
						];
						)*)
					];		
	
	(* ********************************************************************** *)
	
					(*make sure point 2 and 3 are duplicates.  This only works if d2 is not literally the same vertex as d3 (ie 3,3) *)
					(*TODO: remove all code before this that reassigns d3*)
						If[FreeQ[getDuplicateVertices[duplicateVertices, d3], d2],
							(
							If[d3 == duplicatePair[[2]][[1]],
								(
								point4 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]];
								point3 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]]; 			
								p4 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
								p3 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
								
								d4 = duplicatePair[[2]][[1]];
								d3 = duplicatePair[[2]][[2]];
								
								zip = 1; (*zip*)
								
								(*Print["3.1"];*)
								),
								(
								point4 = r2[tempCoordinates[[duplicatePair[[2]][[2]] ]]];
								point3 = r2[tempCoordinates[[duplicatePair[[2]][[1]] ]]]; 			
								p4 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
								p3 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
								
								d4 = duplicatePair[[2]][[2]];
								d3 = duplicatePair[[2]][[1]];
								
								zip = 1; (*zip*)
	
								(*Print["3.2"];*)
								)							
							];	
							)
						];
			
					(*This second vector is NOT necessarily the second duplicate edge -- it's a vector from a point on the first edge to a point on the second edge*)
					vec2 = point3 - point4;
					
					(*get the length of the second edge (should be a number) *)
				(*	p3 = tempCoordinates[[duplicatePair[[2]][[1]] ]];
					p4 = tempCoordinates[[duplicatePair[[2]][[2]] ]];
					v = p4-p3;
					length2 = Sqrt[v[[1]]^2 + v[[2]]^2 + v[[3]]^2];*)
					
					
					(*NOTE: dist is used instead of vec1 and vec2 in the new equation: vec1 and vec2 are worthless.  
					TODO: discard vec1 and vec2*)
					diff = point2 - point3;
					dist = diff[[1]]^2  + diff[[2]]^2 + diff[[3]]^2;
					
	
					(*Print["d2"];
					Print[d2];
					Print["d3"];
					Print[d3];*)
									
					(*Find the rotation angles now*)
					angle1 = -1;
					angle2 = -1;
					(*Courtesy of
					https://mathematica.stackexchange.com/questions/134963/using-the-output-of-reduce
					Converts inequalities to intervals.  It's the only way I could find to generically handle equalities and inequalities, because
					I couldn't figure out how to make Mathematica test an answer to see if it's an inequality or equality.  ToRules[] doesn't accept
					inequalities.
					*)
					ineqToIntv = 
						 HoldPattern[
						   Inequality[m_, Less | LessEqual, s_Symbol, Less | LessEqual, M_] | 
						    Inequality[M_, Greater | GreaterEqual, s_Symbol, 
						     Greater | GreaterEqual, m_]] :> (s == Interval[{m, M}]);
						     
					(*Find when the edges are parallel.  At that point, they are aligned with each other.  There are three cases:
						1: both axes unused, so both are free to rotate
						2: second axis previously used, so only first axis is free to rotate					
						3: first axis previously used, so only second axis is free to rotate					
					 *)
					(* -----------------------------------------------------------------------
					Case 1: both axes unused, so both are free to rotate
					----------------------------------------------------------------------- *)				
					zip = 1; (*TODO: THIS IS A HACK FOR DEBUGGING PURPOSES*)
					If[firstAxisValid == 1 && secondAxisValid == 1,
						(
						(*Print["Debug: axis 1 and 2 valid"];*)
						(*solutions = Solve[Dot[vec1, vec2] == length1 * length2 , {a, a2}, Reals, GeneratedParameters -> (0 &)];*)
						(*Depending on the "type" of edges we are dealing with, solve using different equations.  The second equation, which uses 
							the cross product of the vectors, should be able to solve both "zipped" and "lid"-type edges, but I couldn't get it to work...
							TODO: replace two equations with one that works in both cases
						*)
						If[ zip == 1,
							(*"zipped" type edges*)
							(
							(*psolutions = Reduce[{Abs[Simplify[Dot[vec1, vec2]]] == 1*length1 * length2,
							0 <= Re[a] < 3, 0 <= Re[a2] < 3},
							{a, a2}, Reals];*)
							psolutions = FindMinimum[{dist, 0 <= a < 3, 0 <= a2 < 3}, {{a, 0}, {a2, 0}}][[2]];						
							(*Courtesy of
							https://mathematica.stackexchange.com/questions/134963/using-the-output-of-reduce
							*)
							(*Format the soluctions so that we can actually use them*)
							(*solutions = {psolutions /. ineqToIntv // ToRules};*)
							solutions = psolutions /. ineqToIntv // ToRules;						
							(*solutions = {ToRules[psolutions]};*)
							),
							(*"lid" type edges*)
							(
							psolutions = Reduce[{ 
							Norm[Cross[vec1, vec2]]^2 < 0.001,
							0 <= Re[a] < 3, 0 <= Re[a2] < 3},
							{a, a2}, Reals];
							(*solutions = {ToRules[psolutions]};*)
							solutions = {psolutions /. ineqToIntv // ToRules};
							)
						];
						
						(*Find the smallest positive pair of angles in the list of solutions.  The list should always contain at least one positive pair of angles!*)
						(*Start the angles with the first pair found in the list*)
			(*			angle1 = a /. FindInstance[solutions[[1]][[1]], a][[1]][[1]];
						angle2 = a2 /. FindInstance[solutions[[1]][[2]], a2][[1]][[1]];*)							
						angle1 = (Min[a /. solutions[[1]][[1]]] + Max[a /. solutions[[1]][[1]]]) / 2;
						angle2 = (Min[a2 /. solutions[[1]][[2]]] + Max[a2 /. solutions[[1]][[2]]]) / 2;		
	
						Do[
							(
							(*temp1 = a /. FindInstance[solutions[[i]][[1]], a][[1]][[1]];
							temp2 = a2 /. FindInstance[solutions[[i]][[2]], a2][[1]][[1]];*)		
							temp1 = (Min[a /. solutions[[i]][[1]]] + Max[a /. solutions[[i]][[1]]]) / 2;
							temp2 = (Min[a2 /. solutions[[i]][[2]]] + Max[a2 /. solutions[[i]][[2]]]) / 2;		
	
	(*						(*Get the next pair of angles from the list*)
							(temp1 =  a /. solutions[[i]][[1]]; 
							temp2 =  a2 /. solutions[[i]][[2]];*)
							(*Are they valid angles and less than the accepted angles, or are the accepted angles invalid (negative)? *)
							If[temp1 >= 0 && temp2 >= 0 && ((angle1 < 0 || angle2 < 0) || (temp1 < angle1 && temp2 < angle2)),
								(angle1 = temp1; angle2 = temp2),
								Null ];
							),
							{i, 2, Length[solutions]}
					 	];
		
						),
						(
						(* -----------------------------------------------------------------------
						Case 2: second axis previously used, so only first axis is free to rotate	
						----------------------------------------------------------------------- *)
						If[firstAxisValid == 1,
							(
							(*Print["Debug: axis 1 valid"];*)
						(*	
							Print[dist];
							Print["d2"];
							Print[d2];
							Print["d3"];
							Print[d3];*)
							(*solutions = Solve[Dot[vec1, vec2] == length1 * length2 , {a}, Reals, GeneratedParameters -> (0 &)];*)
							If[ zip == 1,
								(
								psolutions = FindMinimum[{dist, 0 <= a < 3}, {a, 0}][[2]];
								(*psolutions = Reduce[{Abs[Simplify[Dot[vec1, vec2]]] == 1 *length1 * length2,
								0 <= Re[a] < 3},
								{a}, Reals];*)
								(*solutions = {ToRules[psolutions]};*)
								solutions = psolutions /. ineqToIntv // ToRules;						
								),
								(
								psolutions = Reduce[{
								Norm[Cross[vec1, vec2]]^2 < 0.001,
								0 <= Re[a] < 3},
								{a}, Reals];
								(*solutions = {ToRules[psolutions]};*)
								solutions = {psolutions /. ineqToIntv // ToRules};
								)
							];
							
							(*Find the smallest positive pair of angles in the list of solutions.  The list should always contain at least one positive pair of angles!*)
							(*Start the angles with the first pair found in the list*)
				(*			If[zip == 1+1 ,
								(
								angle1 = a /. solutions[[1]][[1]]; (*Solutions are listed in the order that you put in the variables into the Solve function*)
								angle2 = 0;		
								),
								(
								angle1 = a /. FindInstance[solutions[[1]][[1]], a][[1]][[1]];
								angle2 = 0;		
								)
							];		*)
							angle1 = (Min[a /. solutions[[1]][[1]]] + Max[a /. solutions[[1]][[1]]]) / 2;
							angle2 = 0;		
					
							(*angle1 = a /. solutions[[1]][[1]]; (*Solutions are listed in the order that you put in the variables into the Solve function*)
							angle2 = 0;	*)	
							Do[
								(*Get the next pair of angles from the list*)
								(
	(*							If[zip == 1+1 ,
									(
									temp1 = a /. solutions[[i]][[1]]; (*Solutions are listed in the order that you put in the variables into the Solve function*)
									),
									(
									temp1 = a /. FindInstance[solutions[[i]][[1]], a][[1]][[1]];
									)
								];*)							
								temp1 = (Min[a /. solutions[[i]][[1]]] + Max[a /. solutions[[i]][[1]]]) / 2;
								
								(*(temp1 =  a /. solutions[[i]][[1]];*) 
								(*Are they valid angles and less than the accepted angles, or are the accepted angles invalid (negative)? *)
								If[temp1 >= 0 && (angle1 < 0  || temp1 < angle1),
									(angle1 = temp1),
									Null ];
								),
								{i, 2, Length[solutions]}
						 	];
		
							),
							(* -----------------------------------------------------------------------
							Case 3: first axis previously used, so only second axis is free to rotate	
							----------------------------------------------------------------------- *)
							(
							(*Print["Debug: axis 2 valid"];*)
							(*solutions = Solve[Dot[vec1, vec2] == length1 * length2 , {a2}, Reals, GeneratedParameters -> (0 &)];*)
							If[ zip == 1,
								(
								psolutions = FindMinimum[{dist, 0 <= a2 < 3}, {a2, 0}][[2]];
								(*psolutions = Reduce[{Abs[Simplify[Dot[vec1, vec2]]] == 1 * length1 * length2,
									0 <= Re[a2] < 3},
									{a2}, Reals];*)
								(*solutions = {ToRules[psolutions]};*)
								solutions = psolutions /. ineqToIntv // ToRules;						
								),
								(
								psolutions = Reduce[{
									Norm[Cross[vec1, vec2]]^2 < 0.001,
									0 <= Re[a2] < 3},
									{a2}, Reals];
								(*solutions = {ToRules[psolutions]};*)
								solutions = {psolutions /. ineqToIntv // ToRules};
								)
							];
	(*						psolutions = Reduce[{Abs[Dot[vec1, vec2]] == Abs[length1 * length2],
								Norm[Cross[vec1, vec2]]^2 < 0.001,
								0 <= Re[a2] < 3},
								{a2}, Reals];
							solutions = {ToRules[psolutions]};
	*)						
							(*Find the smallest positive pair of angles in the list of solutions.  The list should always contain at least one positive pair of angles!*)
							(*Start the angles with the first pair found in the list*)
							(*If[zip == 1+1 ,
								(
								angle1 = 0;
								angle2 = a2 /. solutions[[1]][[1]];		
								),
								(
								angle1 = 0;
								angle2 = a2 /. FindInstance[solutions[[1]][[1]], a2][[1]][[1]];		
								)
							];	*)
							angle1 = 0;
							angle2 = (Min[a2 /. solutions[[1]][[1]]] + Max[a2 /. solutions[[1]][[1]]]) / 2;
	(*						angle1 = 0; (*Solutions are listed in the order that you put in the variables into the Solve function*)
							angle2 = a2 /. solutions[[1]][[1]];*)		
							Do[
								(*Get the next pair of angles from the list*)
								( 
	(*							If[zip == 1+1 ,
									(
									temp2 = a2 /. solutions[[i]][[1]];		
									),
									(
									temp2 = a2 /. FindInstance[solutions[[i]][[1]], a2][[1]][[1]];		
									)
								];*)	
								temp2 = (Min[a2 /. solutions[[i]][[1]]] + Max[a2 /. solutions[[i]][[1]]]) / 2;
								
								(*temp2 =  a2 /. solutions[[i]][[1]];*)
								(*Are they valid angles and less than the accepted angles, or are the accepted angles invalid (negative)? *)
								If[temp2 >= 0 && (angle2 < 0 || temp2 < angle2),
									(angle2 = temp2),
									Null ];
								),
								{i, 2, Length[solutions]}
						 	];
		
							)
						];
						)
					];
					
		(*			Print["a"];
					Print[angle1];
					Print["a2"];
					Print[angle2];*)
					
	(*				If[MemberQ[duplicatePair, {6,1}],
						(
	
						
						)
					];*)
(*						Print[duplicatePair];
						(*Print[StringForm["point2: ``", p2 ]];*)
						(*Print[StringForm["point3: ``", p3 ]];*)
						Print[StringForm["d2: ``", d2 ]];					
						Print[StringForm["d3: ``", d3 ]];
						(*Print[StringForm["diff: ``", diff ]];*)
						Print[StringForm["angle1: ``", angle1 ]];
						Print[StringForm["angle2: ``", angle2 ]];
						Print[StringForm["firstAxisValid: ``", firstAxisValid]];
						Print[StringForm["secondAxisValid: ``", secondAxisValid]];
						Print[StringForm["axes[[t]][[t2]]: ``", axes[[t]][[t2]] ]];
						Print[StringForm["axes[[t3]][[t4]]: ``", axes[[t3]][[t4]] ]];
						Print[StringForm["first axis faces: ``", data[[1]][[2]]]];
						Print[StringForm["second axis faces: ``", data[[2]][[2]]]];
						Print[StringForm["duplicate vertices: ``", duplicateVertices]];
									*)
					(*Do the animation if the user wanted it*)
					frames = {};
					If[verbose > 0,
						(
						(*Rotate the first faces by angle1, and the second faces by angle2*)
						(*Get points in first faces*)
						firstPoints = getPointsFromFaces[faces, data[[1]][[2]]];
						(*Get points in second faces*)
						secondPoints = getPointsFromFaces[faces, data[[2]][[2]]];
						(*Rotate the points*)
		
		
						Do[
							fakeR1 = RotationTransform[-angle1(j/verbose), firstAxis, firstAnchor];
							fakeR2 = RotationTransform[-angle2(j/verbose), secondAxis, secondAnchor];
							fakeCoordinates = tempCoordinates;
							(
							Do[
								(
								fakeCoordinates = ReplacePart[fakeCoordinates, firstPoints[[i]] -> N[fakeR1[fakeCoordinates[[firstPoints[[i]]]] ]] ];
								),
								{i, 1, Length[firstPoints]}  
							];
							Do[
								(
								fakeCoordinates = ReplacePart[fakeCoordinates, secondPoints[[i]] -> N[fakeR2[fakeCoordinates[[secondPoints[[i]]]] ]] ];
								),
								{i, 1, Length[secondPoints]}                                      
							];					
	
	
							(*export to filepath specified*)
							AppendTo[frames, displayShape[fakeCoordinates, faces, bounds]];
							(*string = ToString[StringJoin[filePath, "''.gif"]];
							Export[ToString[
								  StringForm[
									   filePath<>"``.gif", foldNumber*100 + j]], displayShape[fakeCoordinates, faces]];*)
							),
							{j, 1, verbose}                                      
						];
	
						),Null
					];
	
					
					(*Recreate the transformations, this time with real angles*)
					r1 = RotationTransform[-angle1, firstAxis, firstAnchor];
					r2 = RotationTransform[-angle2, secondAxis, secondAnchor];
					
					(*OLD, never worked
					
					eq1 = N[r1[tempCoordinates[[duplicatePair[[1]]]]]]; (*First system of equations*)
					eq2 = N[r2[tempCoordinates[[duplicatePair[[2]]]]]]; (*Second system of equations*)
					
					Print[eq1];
					Print[eq2];
					(*Only solve this equations for the real solutions*)
					angle1 = Solve[{eq1[[1]] == eq2[[1]],
									eq1[[2]] == eq2[[2]],
									eq1[[3]] == eq2[[3]]}, a];
			
					angle2 = Solve[{eq1[[1]] == eq2[[1]],
									eq1[[2]] == eq2[[2]],
									eq1[[3]] == eq2[[3]]}, a2];
			
					r1 = RotationTransform[-angle1, firstAxis, firstAnchor];
					r2 = RotationTransform[-angle2, secondAxis, secondAnchor];
					
					*)
					
		
					
					(*Rotate the first faces by angle1, and the second faces by angle2*)
					(*Get points in first faces*)
					firstPoints = getPointsFromFaces[faces, data[[1]][[2]]];
					(*Get points in second faces*)
					secondPoints = getPointsFromFaces[faces, data[[2]][[2]]];
					(*Rotate the points*)
					Do[
						(
						tempCoordinates = ReplacePart[tempCoordinates, firstPoints[[i]] -> N[r1[tempCoordinates[[firstPoints[[i]]]] ]] ];
						),
						{i, 1, Length[firstPoints]}  
					];
					Do[
						(
						tempCoordinates = ReplacePart[tempCoordinates, secondPoints[[i]] -> N[r2[tempCoordinates[[secondPoints[[i]]]] ]] ];
						),
						{i, 1, Length[secondPoints]}                                      
					];


									
					
					(*align the edge endpoints now*)
					If[firstAxisValid == 1,
						(
						tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[1]][[1]] -> tempCoordinates[[duplicatePair[[2]][[2]] ]] ];
						tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[1]][[2]] -> tempCoordinates[[duplicatePair[[2]][[1]] ]] ];
						),
						(
						tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[2]][[1]] -> tempCoordinates[[duplicatePair[[1]][[2]] ]] ];
						tempCoordinates = ReplacePart[tempCoordinates, duplicatePair[[2]][[2]] -> tempCoordinates[[duplicatePair[[1]][[1]] ]] ];
						)
					];
					

					result = {tempCoordinates};
					
					(*
					Add the axes to the list of used axes.  The joined/used duplicate edges will be updated outside of this function.
					*)
					usedAxesCopy = usedAxes;
					AppendTo[usedAxesCopy , axes[[t]][[t2]]];
					AppendTo[usedAxesCopy , axes[[t3]][[t4]]];
					AppendTo[usedAxesCopy , {axes[[t]][[t2]][[2]], axes[[t]][[t2]][[1]]}];
					AppendTo[usedAxesCopy , {axes[[t3]][[t4]][[2]], axes[[t3]][[t4]][[1]]}];
		
					(*Add the modified used axes list to the result*)
					AppendTo[result, usedAxesCopy];
					
					(*Modify joined points list*)
					joinedPointsCopy = joinedPoints;
					AppendTo[joinedPointsCopy, {duplicatePair[[1]][[1]], duplicatePair[[2]][[2]]}];
					AppendTo[joinedPointsCopy, {duplicatePair[[1]][[2]], duplicatePair[[2]][[1]]}];
					AppendTo[result, joinedPointsCopy];
					AppendTo[result, 1]; (*fold occurred*)
					AppendTo[result, frames];
					
					)
				];
				
				)
			];				
			
			


			)
		];
		
		Return[result]
	]


(*returns {coordinates, frames}  *)
alignShape[faces_, adjacentFaces_, coordinates_, verbose_]:=
	Module[{normal, normal2, rotateZ, rotateY, zAngle, yAngle, halfTime1, halfTime2,
		faceList, distance, rT, rAngle, normalStart, rotationAxis, min, max, firstPoints, r1, fakeR1, fakeCoordinates, frames, output, i, j, tempCoordinates},
		(*Align the first face with the xy plane*)
		
		tempCoordinates = coordinates;
		frames = {};
		(*get normal of first face*)					
		normalStart = N[Normalize[Cross[tempCoordinates[[faces[[1]][[3]] ]] - tempCoordinates[[faces[[1]][[2]] ]], tempCoordinates[[faces[[1]][[1]] ]] - tempCoordinates[[faces[[1]][[2]] ]] ] ]];
		normal = normalStart;
		(*Print[StringForm["normal: ``", normal]];*)
							
		rotateZ = RotationTransform[z, {0,0,1}, {0,0,0}];
		rotateY = RotationTransform[y, {0,1,0}, {0,0,0}];
		
		normal2 = rotateZ[normal];					
		(*max = NMaxValue[{ Dot[normal2, {1,0,0}], -Pi <= z < Pi}, z];
		zAngle = z /. Last[NSolve[{ Dot[normal2, {1,0,0}] == max, -Pi <= z < Pi}, z]];*)
		zAngle = z /. Last[FindMaximum[{ Dot[normal2, {1,0,0}], -Pi <= z < Pi}, {z,0}]];
		rotateZ = RotationTransform[zAngle, {0,0,1}, {0,0,0}];
		normal = rotateZ[normal];

	(*	Print[StringForm["normal2: ``", normal2]];
		Print[StringForm["zAngle: ``", zAngle]];
		Print[StringForm["normal: ``", normal]];*)
		
		normal2 = rotateY[normal];
		distance = normal2[[1]]^2 + normal2[[2]]^2 + (-1-normal2[[3]])^2;
		(*find minimum (basically 0) *)
		(*min = NMinValue[{distance, -Pi <= y < Pi }, y];*)
		
		(*yAngle = y /. Last[FindMaximum[{ Dot[-normal2, {0,0,1}], -Pi <= y < Pi}, {y, -Pi}]];*)
		(*yAngle = y /. Last[NSolve[{ distance == min, -Pi <= y < Pi}, y]];*)
		yAngle = y /. Last[FindMinimum[{ distance, -Pi <= y < Pi}, {y,0}]];

		rotateY = RotationTransform[yAngle, {0,1,0}, {0,0,0}];
		normal = rotateY[normal];
	(*	Print[StringForm["yAngle: ``", yAngle]];					
		Print[StringForm["normal: ``", normal]];
		Print[StringForm["normal2: ``", normal2]];*)				

		(*get final rotation axis*)
		rotationAxis = Cross[normalStart, normal];
		rT = RotationTransform[zy, rotationAxis, {0,0,0}];
		normal2 = rT[normalStart];
		
	(*	Print[StringForm["normal2: ``", normal2]];*)
			
		distance =  normal2[[1]]^2 + normal2[[2]]^2 + (-1 - normal2[[3]])^2; 
		(*min = NMinValue[{distance, -Pi <= zy < Pi }, zy];
		rAngle = zy /. Last[NSolve[{ distance == min}, zy]];*)					
		rAngle = zy /. Last[FindMinimum[{ distance}, {zy,0}]];
		
		rT = RotationTransform[rAngle, rotationAxis, {0,0,0}];
		normal = rT[normalStart];					
		(*Print[StringForm["rAngle: ``", rAngle]];					
		Print[StringForm["rotationAxis: ``", rotationAxis]];					
		Print[StringForm["normal: ``", normal]];*)
		
		
		halfTime1 = Abs[Round[rAngle * 10, 1]];
		If[halfTime1 <= 0 && zAngle >= 1, halfTime1 = 1];
		(*halfTime2 = Abs[Round[zAngle * 10, 1]];
		If[halfTime2 <= 0 && zAngle >= 1, halfTime2 = 1];*)
		
		faceList = {};
		Do[
			(
			AppendTo[faceList, j];
			),
			{j, 1, Max[adjacentFaces, 2]}
		];
		firstPoints = getPointsFromFaces[faces, faceList];
		
		(*Do the animation if the user wanted it*)
		If[verbose > 0,
			(
			(*Rotate the faces by zAngle first*)
		(*	Print["First Point"];
			Print[firstPoints];
			Print[halfTime1];*)
			
			Do[
				fakeR1 = RotationTransform[rAngle*(j/halfTime1), rotationAxis, tempCoordinates[[faces[[1]][[2]] ]]];
				fakeCoordinates = tempCoordinates;
				(
				Do[
					(
					fakeCoordinates = ReplacePart[fakeCoordinates, firstPoints[[i]] -> N[fakeR1[fakeCoordinates[[firstPoints[[i]]]] ]] ];
					),
					{i, 1, Length[firstPoints]}  
				];

				(*add frame to list*)
				AppendTo[frames, displayShape[fakeCoordinates, faces, {}]];
				),
				{j, 1, halfTime1}
			];
			)
		];

		(*Recreate the transformation, this time with real angles*)
		r1 = RotationTransform[rAngle, rotationAxis, tempCoordinates[[faces[[1]][[2]] ]]];

		(*Rotate the first faces by angle1, and the second faces by angle2*)					
		(*Rotate the points by zAngle*)
		Do[
			(
			tempCoordinates = ReplacePart[tempCoordinates, firstPoints[[i]] -> N[r1[tempCoordinates[[firstPoints[[i]]]] ]] ];
			),
			{i, 1, Length[firstPoints]}  
		];

		output = {tempCoordinates, frames};
		Return[output];			
	];

(* Sets the endpoint coordinates of the first edge equal to the endpoints of the second edge, making them equivalent.
	Parameters:
	edge1 - first edge, with endpoints in clockwise order relative to its face 
	edge2 - second edge, with endpoints in clockwise order relative to its face.
	
	Output:
	

*)
fixEdges[edge1_, edge2_] :=
Module [{result, i, j},
	Null
]


(*Gets the distinct points from a set of faces.
	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is adjacent to the next, in clockwise order!
	
	Output
	list of distinct vertices in the faces
*)
getPointsFromFaces[faces_, facesList_] :=
	Module [{result, i, j},
		result = {};
		
		Do[
			(
				Do[
					(
						If[FreeQ[result, faces[[facesList[[i]] ]][[j]]], AppendTo[result, faces[[facesList[[i]] ]][[j]] ], Null];
					),
					{j, 1, Length[faces[[facesList[[i]] ]] ]}
				];

			),
			{i, 1, Length[facesList]}
		];
		
		Return[result];
	]
