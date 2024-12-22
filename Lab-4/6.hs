type Student = (String, Int, [Int])

averageMarks :: [Int] -> Double
averageMarks marks = fromIntegral (sum marks) / fromIntegral (length marks)

main :: IO ()
main = do
    let students = [("R Subramanian", 101, [85, 90, 78]),
                    ("Manisk", 102, [70, 75, 80]),
                    ("Johnrex", 103, [95, 88, 92])]
    mapM_ (\(name, _, marks) ->
            putStrLn $ name ++ "'s average marks: " ++ show (averageMarks marks)) students
