
data Alfajor = Alfajor {
nombre :: String,
dulzor :: Float,
peso :: Float,
capasDeRelleno :: [Relleno]
} deriving (Show)





data Relleno = Mousse |  DulceDeLeche |  Fruta deriving (Show, Eq)


-- ====================== PUNTO 1 A
jorgito = Alfajor{
nombre = "Jorgito",
capasDeRelleno = [DulceDeLeche],
peso = 80,
dulzor = 8
}

havanna = Alfajor {
nombre = "Havanna",    
capasDeRelleno = [Mousse, Mousse],
peso = 60,
dulzor = 12
}

capitanDelEspacio = Alfajor{
nombre = "Capitan del espacio",
capasDeRelleno = [DulceDeLeche],
peso = 40,
dulzor = 12
}

-- ========================= PUNTO 1 B 

coeficienteDedulzor :: Alfajor -> Float 
coeficienteDedulzor unAlfajor = dulzor unAlfajor / peso unAlfajor

precioDelAlfajor :: Alfajor -> Float 
precioDelAlfajor unAlfajor =  (2*  peso unAlfajor) + (precioRelleno $ capasDeRelleno unAlfajor)


precioRelleno :: [Relleno] -> Float
precioRelleno = sum . map valorUnitario

valorUnitario :: Relleno -> Float 
valorUnitario unRelleno 
    | unRelleno == DulceDeLeche = 12
    | unRelleno == Mousse = 15
    | otherwise = 10 

esPotable :: Alfajor -> Bool 
esPotable (Alfajor _ dulzor peso capasDeRelleno ) = capasDeRelleno /= [] && mismoSabor capasDeRelleno && dulzor >= 0.1

mismoSabor :: [Relleno] -> Bool 
mismoSabor rellenos = all (== head rellenos) rellenos 

-- PUNTO 2
