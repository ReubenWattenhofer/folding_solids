# Overview #

Ever wanted to get the coordinates of a truncated icosahedron?  So did I!  This Mathematica package does three
things:
	1. Returns the coordinates of a 3D shape
	2. Saves an animation (.gif file) of the shape being created
	3. Saves and returns an STL file of the shape, to do with as you please (they're fun to 3D print)



# Dependencies #

This is a Mathematica package, and requires Mathematica.  Unless there's a cloud service I'm unaware of, you'll
need to have Mathematica installed on your computer.



# How to use #

1. Open the package and click "run all code".  This will load the functions.
2. There are two main functions available:
	- polyhedralGraphToShape[]
	- netToShape[]

	I recommend using polyhedralGraphToShape, since it requires a lot less work than netToShape.
	See the samples folder in this repository for examples of each function being used.


	# Using polyhedralGraphToShape #	

	This function requires a planar polyhedral graph of the solid in question.
	
	Here's a link for more information about polyhedral graphs:
		http://mathworld.wolfram.com/PolyhedralGraph.html
	
	By "planar", I mean that no lines can cross in the graph.  Planar graphs have faces, and that is
	how the graph will be input into the function (don't forget the	face which surrounds the entire graph!)	

	Call the function like this:
		polyhedralGraphToShape[faces, edgeLength, filePath, verbose]

	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is
		adjacent to the next.  Winding order (clockwise vs counterclockwise) is irrelevant.
	edgeLength - length of each edge
	filePath - can be absolute or relative, must be a string formatted so Mathematica can read it.
		If relative, Mathematica will assume root is Documents directory (for Windows anyway)
	verbose - number of frames per fold animation.  0 indicates no animation should be created



	# using netToShape #	

	Only recommended if you don't have a polyhedral graph of the solid handy, or if you want to fold
	a particular net.  Be warned that the net will be altered if any vertex has degree >= 5.

	Here's a link for more information about nets:
		http://mathworld.wolfram.com/Net.html

	Call the function like this:
		netToShape[faces, adjacentFaces, duplicates, edgeLength, filePath, verbose]

	Parameters:
	faces - vertices which comprise each face {{1,2,3,4},...} -- assumption is that each vertex is
		adjacent to the next, in clockwise order!
	adjacentFaces - dual of input net, where each face is a vertex whose value is its position in
		the faces list.  Format like {{1,2}, {2,3}, {2,4},..}, where each pair is two adjacent faces
	duplicates - duplicate edges in graph, with endpoints in clockwise order
	edgeLength - length of each edge
	filePath - can be absolute or relative, must be a string formatted so Mathematica can read it.
		If relative, Mathematica will assume root is Documents directory (for Windows anyway)
	verbose - number of frames per fold animation.  0 indicates no animation should be created



# Limitations/errata #

1. This package was designed for convex polyhedra, i.e. the shape must have no dimples/craters in the surface.
2. It should work perfectly for so-called Johnson solids, but "near-Johnson" solids will end up with
	distorted, non-planar faces.  I.e, The result will be a concave shape.
3. I don't think an icosahedron can be folded with the current algorithm, unless there's a way to create a 
	net where no vertex has degree >= 5.


# Miscellaneous #
There are many functions in this package, and a few can be used as general-purpose utility functions.  Here's
a list of them, in the order that they appear in the code.  I ommitted functions which Mathematica already
provides, to the best of my knowledge.  This is not a complete list of the functions in the package.

	getDuplicateVertexList	 (*Finds all the duplicate vertices in a net*)
	getNetHalves		 (*Returns the faces belonging to either side after the connection between
				  two faces is removed*)
	isEdgeInFace		 (*Returns true if an edge is in a face, False otherwise*)
	getDuplicateVertices	 (*Finds all equivalencies of a specified vertex/number/object*)
	getVertexNeighborsInFace (*Gets the neigbors of a vertex in a face -- there will always be two.*)
	createNetCoordinates	 (*Creates the coordinates of a specified net*)
	findAdjacentFaces	 (*finds faces which share at least one vertex*)
	findTrueAdjacentFaces	 (*finds faces which share an edge*)

	
# Credits #

Dr. Ed Aboufadel, from Grand Valley State University, for the project idea and providing mentorship
throughout the project.

