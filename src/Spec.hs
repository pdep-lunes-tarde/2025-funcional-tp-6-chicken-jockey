module Spec where

import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5 - Hamburguesas" $ do

        it "El cuarto de libra tiene precio total 54" $ do
            precioTotal cuartoDeLibra `shouldBe` 54
            -- base 20 + (2 + 20 + 10 + 2)

        it "La pdepBurger tiene precio total 110" $ do
            precioTotal pdepBurger `shouldBe` 110
            -- base 20 * 0.8 = 16 + ingredientes suman 94

        it "El dobleCuarto tiene precio total 84" $ do
            precioTotal dobleCuarto `shouldBe` 84

        it "El bigPdep tiene precio total 89" $ do
            precioTotal bigPdep `shouldBe` 89

        it "Una dobleCuarto del día tiene precio total 88" $ do
            precioTotal (delDia dobleCuarto) `shouldBe` 88

        it "hacerVeggie reemplaza correctamente los ingredientes base" $ do
            let hamburguesa = Hamburguesa 0 [Carne, Pollo, Cheddar, Panceta, Pan]
            ingredientes (hacerVeggie hamburguesa)
                `shouldBe` [PatiVegano, PatiVegano, QuesoDeAlmendras, BaconDeTofu, Pan]

        it "cambiarPanDePati reemplaza los panes por pan integral" $ do
            let hamburguesa = Hamburguesa 0 [Pan, Carne, Pan]
            ingredientes (cambiarPanDePati hamburguesa)
                `shouldBe` [PanIntegral, Carne, PanIntegral]

        it "El dobleCuartoVegano tiene precio total 86" $ do
            precioTotal dobleCuartoVegano `shouldBe` 86

        it "Agrandar una hamburguesa veggie agrega otro PatiVegano" $ do
            let hamb = Hamburguesa 0 [PatiVegano]
            ingredientes (agrandar hamb) `shouldBe` [PatiVegano, PatiVegano]
