module Main

import Network.Socket as Skt
import System as Sys
import System.File as F

import JSON.Derive

%language ElabReflection

record Coordinate where
  constructor MkCoordinate
  x : Double
  y : Double
  z : Double

%runElab derive "Coordinate" [Show,Eq,ToJSON,FromJSON]

data Res : Type where
  MkRes :  Double
        -> Double
        -> Double
        -> Nat
        -> Res

calc :  String 
     -> IO Coordinate
calc f = do
  coords                           <- coordinates f
  let (MkRes xsum ysum zsum count) = op (MkRes 0 0 0 0) coords
      c                            = cast count
  pure $
    MkCoordinate (floor $ xsum / c)
                 (floor $ ysum / c)
                 (floor $ zsum / c)
  where
    coordinates :  String
                -> IO Coordinate
    coordinates cs =
      case decode {a = Coordinate} cs of
        Left  err   =>
          die $ "Could not parse input: " ++ show err
        Right coord =>
          pure coord
    op :  Res
       -> Coordinate
       -> Res
    op (MkRes xsum ysum zsum count)
       (MkCoordinate x y z) =
         MkRes (xsum  + x)
               (ysum  + y)
               (zsum  + z)
               (count `plus` 1)

verify :  (Coordinate, String)
       -> IO ()
verify (right, v) = do
  left <- calc v
  case left /= right of
    True  =>
      pure ()
    False =>
      die $ show left ++ "!= " ++ show right

partial
notify :  String
       -> IO ()
notify msg = do
  Right skt <- socket AF_INET Stream 0
    | Left err => die $ "Error in call to socket: " ++ show err
  _ <- connect skt (Hostname "localhost") 9001
  Right _ <- send skt msg
    | Left err => die $ "Error in call to send: " ++ show err
  close skt

partial
main : IO ()
main =
  let right             = MkCoordinate 2.0 0.5 0.25
      verficationpair1  = "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}"
      verficationpair2  = "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}" 
      verificationpairs : List (Coordinate, String)
      verificationpairs = map (\x => (right, x))
                              [ verficationpair1
                              , verficationpair2
                              ]
    in do
      () <-
        for_ verificationpairs $ \currentpair =>
          verify currentpair
      Right src <- readFile "/tmp/1.json"
        | Left err => die $ "Error reading file: " ++ show err
      pid <- getPID
      notify $ "Idris \t" ++ show pid
      results <- calc src
      notify "stop"
      print $ show results
