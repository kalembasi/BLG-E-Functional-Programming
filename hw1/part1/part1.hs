--define a point
type Point = (Float, Float, Float)

--define a triangle
type Triangle = (Point, Point, Point)

--define a shape list of triangles
type Shape = [Triangle]

--function calculates the midpoint between two points
midPoint :: Point -> Point -> Point
midPoint (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)

--function to subdivide a triangle into smaller triangles
subdivideTriangle :: Triangle -> [Triangle]
subdivideTriangle (p1, p2, p3) = 
    [(p1, m12, m13), (m12, p2, m23), (m13, m23, p3)]  --subdividing into three triangles
    where
        m12 = midPoint p1 p2  --mid between p1 and p2
        m13 = midPoint p1 p3  --mid between p1 and p3
        m23 = midPoint p2 p3  --mid between p2 and p3

--recursive function creates sierpinski triangle
sierpinskiModel :: Int -> Shape -> Shape
sierpinskiModel 0 shape = shape  --initial triangle
sierpinskiModel n shape = sierpinskiModel (n - 1) (concatMap subdivideTriangle shape)  --recursion

--function generates a sierpinski triangle from t1
generateSierpinski :: Int -> Shape
generateSierpinski n = sierpinskiModel n [t1]
    where
        t1 = ((0,0,10),(0,10,0),(10,0,0))  --initial t1

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

-- Main function to generate and write a sierpinski triangle to an STL file
main :: IO ()
main = writeObjModel (generateSierpinski 3) "part1.stl" --generates with 3 iterations
