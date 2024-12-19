max' x y z =  let x1 = (read x)::Double
                  y1 = (read y)::Double
                  z1 = (read z)::Double
            in(max (max x1  y1) z1)

ioMaxLine' = do
            putStrLn "Provide three numbers to be compared:"
            x <- getLine
            y <- getLine
            z <-getLine
            print (max' x y z)