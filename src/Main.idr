module Main

import Data.IORef as R
import Data.Linear.Ref1
import Data.Linear.Traverse1
import Derive.Prelude
import JSON.Parser
import Network.Socket as Skt
import System as Sys
import System.File as F

%language ElabReflection

record Coordinate where
  constructor MkCoordinate
  x : Double
  y : Double
  z : Double

%runElab derive "Coordinate" [Show,Eq]

data Res : Type where
  MkRes :  Double
        -> Double
        -> Double
        -> Nat
        -> Res

op : Ref s Res -> Coordinate -> F1' s
op res (MkCoordinate x y z) t =
  let (MkRes xsum ysum zsum count) # t := read1 res t
    in mod1 res (\(MkRes xsum ysum zsum count) =>
                  MkRes (xsum  + x) (ysum  + y) (zsum  + z) (count `plus` 1)
                ) t

aggregateLoop : Ref s Res -> List JSON -> F1' s
aggregateLoop res []      t = () # t
aggregateLoop res (x::xs) t =
  case x of
    JObject x' =>
      case x' of
        (("x", (JDouble xval)) :: ("y", (JDouble yval)) :: ("z", (JDouble zval)) :: _) =>
          let _ # t := op res (MkCoordinate xval yval zval) t
            in aggregateLoop res xs t
        (("x", (JDouble xval)) :: ("z", (JDouble zval)) :: ("y", (JDouble yval)) :: _) =>
          let _ # t := op res (MkCoordinate xval yval zval) t
            in aggregateLoop res xs t
        (("y", (JDouble yval)) :: ("x", (JDouble xval)) :: ("z", (JDouble zval)) :: _) =>
          let _ # t := op res (MkCoordinate xval yval zval) t
            in aggregateLoop res xs t
        (("y", (JDouble yval)) :: ("z", (JDouble zval)) :: ("x", (JDouble xval)) :: _) =>
          let _ # t := op res (MkCoordinate xval yval zval) t
            in aggregateLoop res xs t
        (("z", (JDouble zval)) :: ("x", (JDouble xval)) :: ("y", (JDouble yval)) :: _) =>
          let _ # t := op res (MkCoordinate xval yval zval) t
            in aggregateLoop res xs t
        (("z", (JDouble zval)) :: ("y", (JDouble yval)) :: ("x", (JDouble xval)) :: _) =>
          let _ # t := op res (MkCoordinate xval yval zval) t
            in aggregateLoop res xs t
        _                                                                        =>
         aggregateLoop res xs t 
    _          =>
      aggregateLoop res xs t

aggregate : String -> Ref World Res -> F1' World
aggregate src res t =
  case parseJSON Virtual src of
    Left  err           =>
      ioToF1 (die $ "Could not parse input: " ++ show err) t
    Right (JObject cs') =>
      case cs' of
        (x :: _) =>
          case snd x of
            JArray cs'' =>
              aggregateLoop res cs'' t
            _           =>
              ioToF1 (die "Input not in proper format.") t
        _         =>
          ioToF1 (die "Input not in proper format.") t
    Right _             =>
      ioToF1 (die "Input not in proper format.") t

resf : Ref s Res -> F1 s Coordinate
resf res t =
  let (MkRes xsum ysum zsum count) # t := read1 res t
      c                                := the Double (cast count)
    in MkCoordinate (xsum / c)
                    (ysum / c)
                    (zsum / c) # t

calc : String -> F1 World Coordinate
calc cs t =
  let res   # t := ref1 (MkRes 0 0 0 0) t
      _     # t := aggregate cs res t
      coord # t := resf res t
    in coord # t

verify : (Coordinate, String) -> IO ()
verify (right, v) = do
  left <- runIO (calc v)
  case left /= right of
    True  =>
      die $ show left ++ " != " ++ show right
    False =>
      pure () 

partial
notify : String -> IO ()
notify msg = do
  Right skt <- socket AF_INET Stream 0
    | Left err => die $ "Error in call to socket: " ++ show err
  _ <- connect skt (Hostname "localhost") 9001
  Right _ <- send skt msg
    | Left err => die $ "Error in call to send: " ++ show err
  close skt

main1 : F1' World
main1 t =
  let verificationpairs      : List (Coordinate, String)
      verificationpairs      = map (\x => (MkCoordinate 2.0 0.5 0.25, x)
                                   ) [ "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}"
                                     , "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"
                                     ]
      _                 # t := ioToF1 (for_ verificationpairs $ \verificationpair =>
                                         verify verificationpair  
                                      ) t
      Right src         # t := ioToF1 (readFile "/tmp/1.json") t
        | Left err      # t => ioToF1 (die $ "Error reading file: " ++ show err) t
      pid               # t := ioToF1 getPID t
      _                 # t := ioToF1 (notify $ "Idris: PID " ++ show pid) t
      results           # t := calc src t
      _                 # t := ioToF1 (notify "stop") t
    in ioToF1 (print $ show results) t

main : IO ()
main = runIO main1
