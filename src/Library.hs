module Library where
import PdePreludat
import Data.Bool (Bool)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry |
    QuesoDeAlmendras | Papas | BaconDeTofu | PatiVegano | PanIntegral
    deriving (Eq, Show)


precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo = 10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
precioIngrediente Papas = 10

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

esVegano :: Ingrediente -> Bool
esVegano ingrediente = ingrediente == PatiVegano

agrandar :: Hamburguesa -> Hamburguesa
agrandar (Hamburguesa precioBase ingredientes)
    | any esCarne ingredientes = Hamburguesa precioBase (Carne : ingredientes)
    | any esPollo ingredientes = Hamburguesa precioBase (Pollo : ingredientes)
    | any esVegano ingredientes = Hamburguesa precioBase (PatiVegano : ingredientes)
    | otherwise = Hamburguesa precioBase ingredientes

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingredienteAgregado hamburguesa = hamburguesa {ingredientes = ingredienteAgregado : ingredientes hamburguesa}

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje (Hamburguesa precioBase ingredientes) = Hamburguesa (precioBase * (1 - porcentaje / 100)) ingredientes

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar $ cuartoDeLibra


-- Punto 2

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agrandar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 30 . agregarIngrediente Papas

-- Punto 3

modificarIngredientes :: ([Ingrediente] -> [Ingrediente]) -> Hamburguesa -> Hamburguesa
modificarIngredientes f hamburguesa = hamburguesa { ingredientes = f (ingredientes hamburguesa) }

modificarPrecioBase :: (Number -> Number) -> Hamburguesa -> Hamburguesa
modificarPrecioBase f hamburguesa = hamburguesa { precioBase = f (precioBase hamburguesa) }

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = modificarIngredientes (map reemplazoVegano)

reemplazoVegano :: Ingrediente -> Ingrediente
reemplazoVegano = cambio Carne PatiVegano . cambio Pollo PatiVegano . cambio Cheddar QuesoDeAlmendras . cambio Panceta BaconDeTofu

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati = modificarIngredientes (map reemplazoPan)

reemplazoPan :: Ingrediente -> Ingrediente
reemplazoPan = cambio Pan PanIntegral

cambio :: Eq a => a -> a -> a -> a
cambio original nuevo actual
    | actual == original = nuevo
    | otherwise = actual


dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati . agrandar . hacerVeggie $ dobleCuarto

-- Utilidad: precio total

precioTotal :: Hamburguesa -> Number
precioTotal hamburguesa = precioBase hamburguesa + sum (map precioIngrediente (ingredientes hamburguesa))
