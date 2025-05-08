module Library where
import PdePreludat
import Data.Bool (Bool)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

--Punto 1

esCarne :: Ingrediente -> Bool
esCarne ingrediente = ingrediente == Carne

esPollo :: Ingrediente -> Bool
esPollo ingrediente = ingrediente == Pollo

agrandar :: Hamburguesa -> Hamburguesa
agrandar (Hamburguesa precioBase ingredientes)
    | any esCarne ingredientes = Hamburguesa precioBase (Carne : ingredientes)
    | any esPollo ingredientes = Hamburguesa precioBase (Pollo : ingredientes)
    | otherwise = Hamburguesa precioBase ingredientes

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingredienteAgregado hamburguesa = hamburguesa {ingredientes = ingredienteAgregado : ingredientes}

descuento ::