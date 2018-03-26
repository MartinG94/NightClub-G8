data Cliente = Cliente {
  nombre :: Nombre
  resistencia :: Resistencia
  amigos :: Clientes
} deriving (Show)

type Nombre = String
type Resistencia = Double
type Clientes = [Cliente]

rodri = Cliente "Rodri" 150 []
marcos = Cliente "Marcos" 40 [rodri]
