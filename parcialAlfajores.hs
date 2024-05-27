import Data.List (isInfixOf)



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
dulzor = 8,
peso = 80,
capasDeRelleno = [DulceDeLeche]
}

havanna = Alfajor {
nombre = "Havanna",    
dulzor = 12,
peso = 60,
capasDeRelleno = [Mousse, Mousse]


}

capitanDelEspacio = Alfajor{
nombre = "Capitan del espacio",
dulzor = 12,
peso = 40,
capasDeRelleno = [DulceDeLeche]


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
abaratarAlfajor :: Alfajor -> Alfajor 
abaratarAlfajor unAlfajor = unAlfajor {
peso = peso unAlfajor - 10, 
dulzor = dulzor unAlfajor - 7}

renombrarAlfajor :: String -> Alfajor -> Alfajor 
renombrarAlfajor nombreNuevo unAlfajor = unAlfajor {
nombre = nombreNuevo     
}

agregarCapa :: Relleno -> Alfajor -> Alfajor 
agregarCapa unaCapa unAlfajor = unAlfajor {
capasDeRelleno =  unaCapa : capasDeRelleno unAlfajor     
}

hacerPremium :: Alfajor -> Alfajor 
hacerPremium unAlfajor 
    | esPotable unAlfajor = unAlfajor {capasDeRelleno = head (capasDeRelleno unAlfajor) : capasDeRelleno unAlfajor, nombre = nombre unAlfajor ++ " premium"}
    | otherwise = unAlfajor

muyPremium :: Int -> Alfajor -> Alfajor
muyPremium 0 unAlfajor = unAlfajor 
muyPremium gradoDePremium unAlfajor = muyPremium (gradoDePremium - 1) (hacerPremium unAlfajor)


jorgitito :: Alfajor -> Alfajor 
jorgitito = renombrarAlfajor "jorgitito" . abaratarAlfajor 

jorgelin :: Alfajor -> Alfajor 
jorgelin  = renombrarAlfajor "jorgelin" . agregarCapa DulceDeLeche 

capitanCosta :: Alfajor -> Alfajor 
capitanCosta = renombrarAlfajor "capitan del espacio de costa a costa".muyPremium 4.abaratarAlfajor 


-- PUNTO 3 

data Cliente = Cliente {
nombreCliente :: String,
dineroDisponible :: Float,
criterios :: Criterio,
listaDeAlfajores :: [Alfajor]
}

type Criterio = Alfajor -> Bool 

emi = Cliente {
nombreCliente = "Emi",
dineroDisponible = 120,
criterios = buscaMarca "Capitan del espacio",
listaDeAlfajores = []
}

tomi = Cliente {
nombreCliente = "Tomi",
dineroDisponible = 100,
criterios = criterioTomi "premium",
listaDeAlfajores = []
}

dante = Cliente {
nombreCliente = "dante",
dineroDisponible = 200,
criterios = [not.esPotable, antiRelleno DulceDeLeche],
listaDeAlfajores =[]    
}

juan = Cliente {
nombreCliente = "juan",
dineroDisponible = 500,
criterios = [dulcero, buscaMarca "jorgito", buscaMarca "Premium", antiRelleno Mousse],
listaDeAlfajores = []
}


buscaMarca :: String -> Alfajor -> Bool 
buscaMarca unaMarca = isInfixOf unaMarca . nombre  

criterioTomi :: String -> Alfajor -> Bool 
criterioTomi premium unAlfajor = buscaMarca premium unAlfajor && dulcero unAlfajor

dulcero :: Alfajor -> Bool 
dulcero (Alfajor _ dulzor _ _ ) = dulzor > 0.15

criterioDante :: Relleno -> Alfajor -> Bool 
criterioDante unRelleno unAlfajor = (not.esPotable) unAlfajor && antiRelleno unRelleno unAlfajor

antiRelleno :: Relleno -> Alfajor -> Bool 
antiRelleno unRelleno unAlfajor = elem unRelleno (capasDeRelleno unAlfajor)

criterioJuan :: String -> String -> Alfajor -> Bool
criterioJuan unaMarca premium unAlfajor = criterioTomi premium unAlfajor && buscaMarca "jorgito" unAlfajor && antiRelleno Mousse unAlfajor


-- PUNTO 3 B 
leGustoOnoLeGusto :: Cliente -> [Alfajor] -> [Alfajor] 
leGustoOnoLeGusto unCliente = filter (criterios unCliente) 


-- PUNTO 3 C
comprarAlfajor :: Cliente -> Alfajor -> Cliente
comprarAlfajor unCliente unAlfajor 
    | precioDelAlfajor unAlfajor > dineroDisponible unCliente = unCliente
    | otherwise = (agregar unAlfajor . gastar unAlfajor)  unCliente 


agregar :: Alfajor-> Cliente -> Cliente
agregar unAlfajor unCliente = unCliente {listaDeAlfajores = unAlfajor : listaDeAlfajores unCliente} 

gastar :: Alfajor -> Cliente -> Cliente
gastar unAlfajor unCliente = unCliente{dineroDisponible = dineroDisponible unCliente - precioDelAlfajor unAlfajor}

-- PUNTO 3 D 
compraMayorista :: [Alfajor] -> Cliente -> Cliente
compraMayorista alfajores unCliente = foldl comprarAlfajor unCliente ( leGustoOnoLeGusto unCliente alfajores) 