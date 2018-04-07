{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

type Nombre = String
type Dinero = Float
type Evento = Usuario -> Usuario
type Transacción = Usuario -> Evento

data Usuario = Usuario {
nombre :: Nombre,
billetera :: Dinero
} deriving(Show,Eq)

nuevoNombre otroNombre usuario = usuario {nombre = otroNombre}

nuevoSaldo otroSaldo usuario = usuario {billetera = otroSaldo}

pepe = Usuario {nombre = "Jose", billetera = 10}
pepe2 = Usuario {nombre = "Jose", billetera = 20}
lucho = Usuario {nombre = "Luciano", billetera = 2}
luxito = Usuario {nombre = "Jorge", billetera = 5}

deposito :: Dinero -> Evento
deposito dineroDepositado usuario = nuevoSaldo (billetera usuario + dineroDepositado) usuario

extraccion :: Dinero -> Evento
extraccion dineroARetirar usuario = nuevoSaldo ( max 0 (billetera usuario - dineroARetirar) ) usuario

upgrade :: Evento
upgrade usuario = nuevoSaldo (billetera usuario + verificarUpgrade usuario) usuario

verificarUpgrade usuario
      | billetera usuario * 0.2 > 10 = 10
      | otherwise = billetera usuario * 0.2

cierreDeCuenta :: Evento
cierreDeCuenta usuario = nuevoSaldo 0 usuario

quedaIgual :: Evento
quedaIgual usuario = usuario

verificarUsuario usuarioAComparar usuario = nombre usuarioAComparar == nombre usuario

transacción1 :: Transacción
transacción1 usuario
      | verificarUsuario lucho usuario = cierreDeCuenta
      | otherwise = quedaIgual

transacción2 :: Transacción
transacción2 usuario
      | verificarUsuario pepe usuario = deposito 5
      | otherwise = quedaIgual

tocoYMeVoy :: Evento
tocoYMeVoy usuario = (cierreDeCuenta . upgrade . deposito 15) usuario

ahorranteErrante :: Evento
ahorranteErrante usuario  = (deposito 10 . upgrade . deposito 8 . extraccion 1 . deposito 2 . deposito 1) usuario

transacción3 :: Transacción
transacción3 usuario
      | verificarUsuario lucho usuario = tocoYMeVoy
      | otherwise = quedaIgual

transacción4 :: Transacción
transacción4 usuario
      |verificarUsuario lucho usuario = ahorranteErrante
      | otherwise = quedaIgual

transacción5 :: Transacción
transacción5 usuario
      | verificarUsuario lucho usuario = deposito 7
      | verificarUsuario pepe usuario = extraccion 7
      | otherwise = quedaIgual

ejecutarTests = hspec $ do
    describe "Verificando resultados de los Eventos con una billetera de saldo 10." $ do
      it "Depositar 10 más: Debería quedar con 20 monedas." $ billetera (deposito 10 pepe) `shouldBe` 20
      it "Extraer 3: Debería quedar con 7 monedas." $ billetera (extraccion 3 pepe) `shouldBe` 7
      it "Extraer 15: Debería quedar con 0 monedas." $ billetera (extraccion 15 pepe) `shouldBe` 0
      it "Un upgrade: Debería quedar con 12 monedas." $ billetera (upgrade pepe) `shouldBe` 12
      it "Cerrar la cuenta: Debería quedar con 0 monedas." $ billetera (cierreDeCuenta pepe) `shouldBe` 0
      it "Queda igual: Debería quedar con 10 monedas." $ billetera (quedaIgual pepe) `shouldBe` 10
      it "Depositar 1000, y luego tener un upgrade: Debería quedar con 1020 monedas." $ billetera ((upgrade.(deposito 1000)) pepe) `shouldBe` 1020

    describe "Verificando usuarios" $ do
      it "¿Cuál es la billetera de Pepe?: Debería ser 10 monedas." $ billetera pepe `shouldBe` 10
      it "¿Cuál es la billetera de Pepe, luego de un cierre de su cuenta?: Debería ser 0." $ billetera (cierreDeCuenta pepe) `shouldBe` 0
      it "¿Cómo quedaría la billetera de Pepe si le depositan 15 monedas, extrae 2, y tiene un Upgrade? Debería quedar en 27.6." $ billetera ((upgrade.extraccion 2.deposito 15) pepe) `shouldBe` 27.6

    describe "Verificando transacciones" $ do
      it "Aplicar la transacción 1 a Pepe. Esto debería producir el evento “Queda igual”, que si se aplicara a una billetera de 20 monedas, deberá dar una billetera con ese mismo monto." $ billetera (transacción1 pepe pepe2) `shouldBe` 20
      it "Aplicar la transacción 2 a Pepe. El resultado, deberá ser el evento de depositar 5 monedas. Aplicarlo a una billetera de 10 monedas, mostrando que queda con 15." $ billetera (transacción2 pepe pepe) `shouldBe` 15
      it "Aplicar la transacción 2 al nuevo Pepe. Aplicar el evento resultante a una billetera de 50 monedas, y verificar que aumenta quedando con 55." $ billetera(transacción2 pepe2 (nuevoSaldo 50 pepe)) `shouldBe` 55
      it "Aplicar la transacción 3 a Lucho. Ver cómo queda una billetera inicial de 10 monedas. Debería quedar con 0" $ billetera(transacción3 lucho pepe)  `shouldBe` 0
      it "Aplicar la transacción 4 a Lucho. Ver cómo queda una billetera inicial de 10 monedas. Debería quedar con 34" $ billetera(transacción4 lucho pepe)  `shouldBe` 34
      it "Aplicar la transacción 5 a Pepe. Debería causar el evento de extracción de 7 unidades. Al aplicarlo a una billetera de 10 monedas, debería dar una nueva billetera de 3." $ billetera(transacción5 pepe pepe) `shouldBe` 3
      it "Aplicar la transacción 5 a Lucho. Debería causar el evento de depósito de 7 unidades. Al aplicarlo a una billetera de 10 monedas, quedando con 17." $ billetera(transacción5 lucho pepe) `shouldBe` 17
