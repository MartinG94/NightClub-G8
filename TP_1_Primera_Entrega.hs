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
      it "Al depositar 10, queda con 20." $ billetera (deposito 10 pepe) `shouldBe` 20
      it "Al extraer 3, queda con 7." $ billetera (extraccion 3 pepe) `shouldBe` 7
      it "Al extraer 15, queda con 0." $ billetera (extraccion 15 pepe) `shouldBe` 0
      it "Con un upgrade, queda con 12." $ billetera (upgrade pepe) `shouldBe` 12
      it "Al cerrar la cuenta, queda con 0." $ billetera (cierreDeCuenta pepe) `shouldBe` 0
      it "Con queda igual, queda con 10." $ billetera (quedaIgual pepe) `shouldBe` 10
      it "Al depositar 1000, y luego tener un upgrade, queda con 1020." $ billetera ((upgrade.(deposito 1000)) pepe) `shouldBe` 1020

    describe "Verificando usuarios" $ do
      it "La billetera de pepe es de 10." $ billetera pepe `shouldBe` 10
      it "La billetera de Pepe, luego de un cierre de cuenta, es de 0." $ billetera (cierreDeCuenta pepe) `shouldBe` 0
      it "La billetera de Pepe si le depositan 15, extrae 2, y tiene un Upgrade, es de 27.6." $ billetera ((upgrade.extraccion 2.deposito 15) pepe) `shouldBe` 27.6

    describe "Verificando transacciones" $ do
      it "Aplicar la transacción 1 a Pepe, esto produce el evento Queda igual, que si se aplica a una billetera de 20, debe dar una billetera con ese mismo monto." $ billetera (transacción1 pepe pepe2) `shouldBe` 20
      it "Aplicar la transacción 2 a Pepe, esto produce el evento depositar 5, que si se aplica a una billetera de 10, queda con 15." $ billetera (transacción2 pepe pepe) `shouldBe` 15
      it "Aplicar la transacción 2 al nuevo Pepe,esto produce un evento, que aplicado a una billetera de 50, queda con 55." $ billetera(transacción2 pepe2 (nuevoSaldo 50 pepe)) `shouldBe` 55
      it "Aplicar la transacción 3 a Lucho. Ver cómo queda una billetera inicial de 10. Debería quedar con 0" $ billetera(transacción3 lucho pepe)  `shouldBe` 0
      it "Aplicar la transacción 4 a Lucho. Ver cómo queda una billetera inicial de 10. Debería quedar con 34" $ billetera(transacción4 lucho pepe)  `shouldBe` 34
      it "Aplicar la transacción 5 a Pepe, esto produce el evento de extracción 7. Al aplicarlo a una billetera de 10, debería dar una nueva billetera de 3." $ billetera(transacción5 pepe pepe) `shouldBe` 3
      it "Aplicar la transacción 5 a Lucho, esto produce el evento de depósito  7. Al aplicarlo a una billetera de 10, debería dar una nueva billetera de 17." $ billetera(transacción5 lucho pepe) `shouldBe` 17
