# Overview #

Ever wanted to get the coordinates of a truncated icosahedron?  So did I!  This Mathematica package does three
things:
	1. Returns the coordinates of a 3D shape
	2. Saves an animation (.gif file) of the shape being created
	3. Saves and returns an STL file of the shape, to do with as you please (they're fun to 3D print)

For more background information on this project, check out this article: 
http://chalkdustmagazine.com/blog/folding-nets-into-johnson-solids/


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
		polyhedralGraphToShape[faces, edgeLength, filePath, verbose, alignFreq]

	Parameters:
	faces - vertices which comprise each face in the polyhedral graph {{1,2,3,4},...} -- assumption is
		that each vertex is adjacent to the next.  Winding order (clockwise vs counterclockwise) is
		irrelevant for every face EXCEPT THE FIRST.  The first face is assumed to wind clockwise.
	edgeLength - length of each edge
	filePath - can be absolute or relative, must be a string formatted so Mathematica can read it.
		If relative, Mathematica will assume root is Documents directory (for Windows anyway)
	verbose - number of frames per fold animation.  0 indicates no animation should be created
	alignFreq - how often to align first face with xy plane 
		0: never
		-1: at end of folding
		x >= 1: every x folds 


	# using netToShape #	

	Only recommended if you don't have a polyhedral graph of the solid handy, or if you want to fold
	a particular net.  Be warned that the net will be altered if any vertex has degree >= 5.

	Here's a link for more information about nets:
		http://mathworld.wolfram.com/Net.html

	Call the function like this:
		netToShape[faces, adjacentFaces, duplicates, edgeLength, filePath, verbose]

	Parameters:
	faces - vertices which comprise each face in the net {{1,2,3,4},...} -- assumption is that each vertex
		is adjacent to the next.  Winding order (clockwise vs counterclockwise) is irrelevant for
		every face EXCEPT THE FIRST.  The first face is assumed to wind clockwise.
	adjacentFaces - dual of input net, where each face is a vertex whose value is its position in
		the faces list.  Format like {{1,2}, {2,3}, {2,4},..}, where each pair is two adjacent faces
	duplicates - duplicate edges in graph, with endpoints in clockwise order
	edgeLength - length of each edge
	filePath - can be absolute or relative, must be a string formatted so Mathematica can read it.
		If relative, Mathematica will assume root is Documents directory (for Windows anyway)
	verbose - number of frames per fold animation.  0 indicates no animation should be created
	alignFreq - how often to align first face with xy plane 
		0: never
		-1: at end of folding
		x >= 1: every x folds 


# Limitations #

1. This package was designed for convex polyhedra, i.e. the shape must have no dimples/craters in the surface.
2. It should work perfectly for so-called Johnson solids, but "near-Johnson" solids will end up with
	distorted, non-planar faces.  I.e, The result will be a concave shape.
3. This algorithm only seems to work for "near-Johnson" solids.  Shapes which require significantly distorted
	faces will probably fail.  For an example, see the Herschel graph polyhedron:
	http://aperiodical.com/wp-content/uploads/2013/09/herschel-enneahedron.png
	(found on http://aperiodical.com/2013/10/an-enneahedron-for-herschel )
4. If a polyhedron does not have a net which has no degree-5 vertices, then it can't be folded using the
	current algorithm.  Here's the current list of polyhedra we found that seem to be unfoldable:
	- Icosahedron
	- Snub Disphenoid


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



# ------------------------------------------------------------- #
#			    Bugs				#
#								#
# (These notes are for myself as much as the general audience)	#
# ------------------------------------------------------------- #

# 5/20/18 #
Alignment:

Sometimes the algorithm fails to align the first face with the xy plane.  If you're getting a lot of errors,
	that's probably why.  Temp fix: Setting the alignFreq parameter to 0, disabling the alignment feature,
	will prevent this.
*Fix pending*


Unfolding:

	recursiveUnfold function:
If a point on a face is changed, but that face is later only visited during a recursive unwind, then the face
	may no longer share two vertices with its parent.  This is irrelevent if the changed point is not one 
	of the two shared vertices, since the other points will be changed again.  Bug found with the Herschel
	polyhedral graph.  Temp fix: use netToShape instead, or input the polyhedral face order differently.
*fix pending*




