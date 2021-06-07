bmi :: Float -> Float -> String
bmi x y = 
    let imc = x / (y ^ 2)
    in if (imc <= 18.5)
       then "ABAIXO"
       else if (imc >= 30)
       then "ACIMA"
       else "NORMAL"

bmi' :: Float -> Float -> String
bmi' x y
    | imc <= 18.5 = "ABAIXO"
    | imc >= 30   = "ACIMA"
    | otherwise   = "NORMAL"
    where imc = x / (y ^ 2)

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
    where digits = take 9 cpf
          dv1    = cpfDV digits [10,9..]
          dv2    = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
    let expr = (sum $ zipWith (*) digits mults) `mod` 11
    in if expr < 2 then 0 else 11 - expr

--andTable :: [(Bool, Bool, Bool)]
