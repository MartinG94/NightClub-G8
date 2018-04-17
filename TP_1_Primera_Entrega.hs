{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

type Billetera = Float
type Evento = Billetera -> Billetera

depósito :: Billetera -> Evento
depósito dineroADepositar billeteraDelUsuario = dineroADepositar + billeteraDelUsuario

extracción :: Billetera -> Evento
extracción dineroARetirar billeteraDelUsuario =  max 0 (billeteraDelUsuario - dineroARetirar)

upgrade :: Evento
upgrade billeteraDelUsuario =  billeteraDelUsuario + min 10 (billeteraDelUsuario * 0.2)

cierreDeCuenta :: Evento
cierreDeCuenta billeteraDelUsuario = 0

quedaIgual :: Evento
quedaIgual = id

pruebasConEventos = hspec $ do
  describe "Pruebas de los eventos con una billetera de saldo 10" $ do
    it "1 - Al depositar 10, queda con 20." $ depósito 10 10 `shouldBe` 20
    it "2 - Al extraer 3, queda con 7." $ extracción 3 10 `shouldBe` 7
    it "3 - Al extraer 15, queda con 0." $ extracción 15 10 `shouldBe` 0
    it "4 - Con un upgrade, queda con 12." $ upgrade 10 `shouldBe` 12
    it "5 - Al cerrar la cuenta, queda con 0." $ cierreDeCuenta 10 `shouldBe` 0
    it "6 - Con queda igual, queda con 10." $ quedaIgual 10 `shouldBe` 10
    it "7 - Al depositar 1000, y luego tener un upgrade, queda con 1020." $ (upgrade.(depósito 1000)) 10 `shouldBe` 1020

data Usuario = Usuario {
  nombre :: Nombre,
  billetera :: Billetera
} deriving(Show,Eq)

nuevoNombre otroNombre usuario = usuario {nombre = otroNombre}
nuevoSaldo otroSaldo usuario = usuario {billetera = otroSaldo}

type Nombre = String

pepe = Usuario "José" 10
lucho = Usuario "Luciano" 2

type Transacción = Usuario -> Evento
type TransacciónGenérica = Usuario -> Evento -> Usuario -> Evento

verificarUsuario usuarioAComparar usuario = nombre usuarioAComparar == nombre usuario

crearUnaNuevaTransacción :: TransacciónGenérica
crearUnaNuevaTransacción usuarioAComparar unEvento usuario
      | verificarUsuario usuarioAComparar usuario = unEvento
      | otherwise = quedaIgual

transacción1 :: Transacción
transacción1 = crearUnaNuevaTransacción lucho cierreDeCuenta

transacción2 :: Transacción
transacción2 = crearUnaNuevaTransacción pepe (depósito 5)

pepe2 = Usuario "José" 20

tocoYMeVoy :: Evento
tocoYMeVoy = cierreDeCuenta . upgrade . depósito 15

ahorranteErrante :: Evento
ahorranteErrante = depósito 10 . upgrade . depósito 8 . extracción 1 . depósito 2 . depósito 1

transacción3 :: Transacción
transacción3 = crearUnaNuevaTransacción lucho tocoYMeVoy

transacción4 :: Transacción
transacción4 = crearUnaNuevaTransacción lucho ahorranteErrante

{-
--Modelar partiendo del ejemplo anterior
transacción5 :: Transacción
transacción5 usuario
      | verificarUsuario lucho usuario = depósito 7
      | verificarUsuario pepe usuario = extracción 7
      | otherwise = quedaIgual

ejecutarTests = hspec $ do
    describe "Pruebas de los eventos con una billetera de saldo 10." $ do
      it "1 - Al depositar 10, queda con 20." $
        billetera (depósito 10 alguienConBilleteraDeSaldo10) `shouldBe` 20
      it "2 - Al extraer 3, queda con 7." $
        billetera (extracción 3 alguienConBilleteraDeSaldo10) `shouldBe` 7
      it "3 - Al extraer 15, queda con 0." $
        billetera (extracción 15 alguienConBilleteraDeSaldo10) `shouldBe` 0
      it "4 - Con un upgrade, queda con 12." $
        billetera (upgrade alguienConBilleteraDeSaldo10) `shouldBe` 12
      it "5 - Al cerrar la cuenta, queda con 0." $
        billetera (cierreDeCuenta alguienConBilleteraDeSaldo10) `shouldBe` 0
      it "6 - Con queda igual, queda con 10." $
        billetera (quedaIgual alguienConBilleteraDeSaldo10) `shouldBe` 10
      it "7 - Al depositar 1000, y luego tener un upgrade, queda con 1020." $
        billetera ((upgrade.(depósito 1000)) alguienConBilleteraDeSaldo10) `shouldBe` 1020

    describe "Verificando usuarios" $ do
      it "8 - La billetera de pepe es de 10." $
        billetera pepe `shouldBe` 10
      it "9 - La billetera de Pepe, luego de un cierre de cuenta, es de 0." $
      --Usar composicion
        billetera (cierreDeCuenta pepe) `shouldBe` 0
      it "10 - La billetera de Pepe si le depositan 15, extrae 2, y tiene un Upgrade, es de 27.6." $
        billetera ((upgrade.extracción 2.depósito 15) pepe) `shouldBe` 27.6

    describe "Pruebas con las transacciones" $ do
      it "11 - Aplicar la transacción 1 a Pepe, esto produce el evento Queda igual, que si se aplica a una billetera de 20, debe dar una billetera con ese mismo monto." $
        billetera (transacción1 pepe alguienConBilleteraDeSaldo20) `shouldBe` 20
      it "12 - Aplicar la transacción 2 a Pepe, esto produce el evento depositar 5, que si se aplica a una billetera de 10, queda con 15." $
        billetera (transacción2 pepe alguienConBilleteraDeSaldo10) `shouldBe` 15
      it "13 - Aplicar la transacción 2 al nuevo Pepe, esto produce un evento, que aplicado a una billetera de 50, queda con 55." $
        billetera(transacción2 pepe2 alguienConBilleteraDeSaldo50) `shouldBe` 55
      it "14 - Aplicar la transacción 3 a Lucho. Ver cómo queda una billetera inicial de 10. Debería quedar con 0" $
        billetera(transacción3 lucho alguienConBilleteraDeSaldo10)  `shouldBe` 0
      it "15 - Aplicar la transacción 4 a Lucho. Ver cómo queda una billetera inicial de 10. Debería quedar con 34" $
        billetera(transacción4 lucho alguienConBilleteraDeSaldo10)  `shouldBe` 34
      it "16 - Aplicar la transacción 5 a Pepe, esto produce el evento de extracción 7. Al aplicarlo a una billetera de 10, debería dar una nueva billetera de 3." $
        billetera(transacción5 pepe alguienConBilleteraDeSaldo10) `shouldBe` 3
      it "17 - Aplicar la transacción 5 a Lucho, esto produce el evento de depósito 7. Al aplicarlo a una billetera de 10, debería dar una nueva billetera de 17." $
        billetera(transacción5 lucho alguienConBilleteraDeSaldo10) `shouldBe` 17
-}
