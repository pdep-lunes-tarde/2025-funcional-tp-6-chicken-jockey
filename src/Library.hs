module Library where
import PdePreludat
import Data.Bool (Bool)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas
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

-- Punto 1

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

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje (Hamburguesa precioBase ingredientes) = Hamburguesa (precioBase * (1 - porcentaje / 100)) ingredientes

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar cuartoDeLibra

-- Punto 2

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agrandar cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

precioIngrediente Papas = 10

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 . agregarIngrediente Papas hamburguesa