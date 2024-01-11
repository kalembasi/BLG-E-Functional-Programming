--define a point
type Point = (Float, Float, Float)

--define a triangle
type Triangle = (Point, Point, Point)

--define a shape list of triangles
type Shape = [Triangle]

--function to convert a triangle into STL format
createTriangleDef :: Triangle -> String
createTriangleDef ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) = 
    "  facet\n" ++
    "    outer loop\n" ++
    "      vertex " ++ show x1 ++ " " ++ show y1 ++ " " ++ show z1 ++ "\n" ++
    "      vertex " ++ show x2 ++ " " ++ show y2 ++ " " ++ show z2 ++ "\n" ++
    "      vertex " ++ show x3 ++ " " ++ show y3 ++ " " ++ show z3 ++ "\n" ++
    "    endloop\n" ++
    "  endfacet\n" 

--function to convert a shape into STL format
createObjectModelString :: Shape -> String 
createObjectModelString n = "solid Object01\n" ++ concatMap createTriangleDef n ++ "endsolid Object01"

--function to write the shape to an STL file
writeObjModel :: Shape -> String -> IO ()
writeObjModel x filename = writeFile filename (createObjectModelString x)

--function creates a cube at a specified position with a given size
makeCube :: Point -> Float -> Shape
makeCube (cx, cy, cz) size =
    let half = size / 2
        --define corners of the cube based on center and half size
        p1 = (cx - half, cy - half, cz - half)
        p2 = (cx + half, cy - half, cz - half)
        p3 = (cx + half, cy + half, cz - half)
        p4 = (cx - half, cy + half, cz - half)
        p5 = (cx - half, cy - half, cz + half)
        p6 = (cx + half, cy - half, cz + half)
        p7 = (cx + half, cy + half, cz + half)
        p8 = (cx - half, cy + half, cz + half)
    in  [ (p1, p2, p3), (p1, p3, p4), --front
          (p5, p6, p7), (p5, p7, p8), --back
          (p1, p5, p8), (p1, p8, p4), --left
          (p2, p6, p7), (p2, p7, p3), --right
          (p1, p2, p6), (p1, p6, p5), --bottom
          (p4, p3, p7), (p4, p7, p8)  --top
        ]

--recursive function createa fractal cubes
createFractalCubes :: Int -> Point -> Float -> Shape
createFractalCubes 0 (x, y, z) f = makeCube (x, y, z) f  --initial cube
createFractalCubes n (x, y, z) f =
  concat
  [
    makeCube (x, y, z) f, 
    --recursively creates smaller cubes around the current cube
    createFractalCubes (n-1) (x+3*f/4, y, z) (f/2),  --cube in positive X direction
    createFractalCubes (n-1) (x-3*f/4, y, z) (f/2),  --cube in negative X direction
    createFractalCubes (n-1) (x, y+3*f/4, z) (f/2),  --cube in positive Y direction
    createFractalCubes (n-1) (x, y-3*f/4, z) (f/2),  --cube in negative Y direction
    createFractalCubes (n-1) (x, y, z+3*f/4) (f/2),  --cube in positive Z direction
    createFractalCubes (n-1) (x, y, z-3*f/4) (f/2)   --cube in negative Z direction
  ]

--function to create a fractal of cubes
cubeFractal :: Int -> Shape
cubeFractal n = createFractalCubes n (0, 0, 0) 10.0  --creates fractal cubes from the origin

--extract the first element of a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

--extract the second element of a triple
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

--extract the third element of a triple
thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z


main :: IO ()
main = do
    let fractal = cubeFractal 2  --change number for more iterations
    writeObjModel fractal "part4.stl"
