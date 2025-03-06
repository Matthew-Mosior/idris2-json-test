module NMain

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
-- %runElab derive "Coordinate" [Show,Eq,ToJSON,FromJSON]

--record Coordinates where
--  constructor MkCoordinates
--  coords : List (Maybe Coordinate)

-- %runElab derive "Coordinates" [Show,Eq]

-- %runElab derive "Coordinates" [Show,Eq,ToJSON,FromJSON]

--coords : Value -> Parser (List Coordinate)
--coords = withObject "coords" (field "coordinates")

data Res : Type where
  MkRes :  Double
        -> Double
        -> Double
        -> Nat
        -> Res

{-
op : Ref s Res -> Ref s Coordinate -> F1' s
op res coord t =
  let (MkCoordinate x y z)         # t := read1 coord t
      (MkRes xsum ysum zsum count) # t := read1 res t
    in mod1 res (\(MkRes xsum ysum zsum count) =>
                  MkRes (xsum  + x) (ysum  + y) (zsum  + z) (count `plus` 1)
                ) t
-}

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
        ((_, (JDouble xval)) :: (_, (JDouble yval)) :: (_, (JDouble zval)) :: _) =>
          let _ # t := op res (MkCoordinate xval yval zval) t
            in aggregateLoop res xs t
        _                                                                        =>
         aggregateLoop res xs t 
    _          =>
      aggregateLoop res xs t

aggregate : String -> Ref World Res -> F1' World
aggregate cs res t =
  case parseJSON Virtual cs of
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
      c                                := cast count
    in MkCoordinate (floor $ xsum / c)
                    (floor $ ysum / c)
                    (floor $ zsum / c) # t

calc : String -> F1 World Coordinate
calc cs t =
  let res   # t := ref1 (MkRes 0 0 0 0) t
      _     # t := aggregate cs res t
      coord # t := resf res t
    in coord # t

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
  let Right src    # t := ioToF1 (readFile "/tmp/1.json") t
        | Left err # t => ioToF1 (die $ "Error reading file: " ++ show err) t
      pid          # t := ioToF1 getPID t
      _            # t := ioToF1 (notify $ "Idris \t" ++ show pid) t
      results      # t := calc src t
      _            # t := ioToF1 (notify "stop") t
    in ioToF1 (print $ show results) t

main : IO ()
main = runIO main1

{-
calc :  String
     -> IO Coordinate
calc f =
  run1 $ \t =>
    let res # t := ref1 (MkRes 0 0 0 0) t
        



  (MkRes xsum ysum zsum count) <- coordinates f
                                              (MkRes 0 0 0 0)
  let c                        =  cast count
  pure $
    MkCoordinate (floor $ xsum / c)
                 (floor $ ysum / c)
                 (floor $ zsum / c)
  where
    coordinates :  String
                -> IO Coordinates
    coordinates cs =


      case parseJSON Virtual cs of
        Left  err           =>
          die $ "Could not parse input: " ++ show err
        Right (JObject cs') =>
          case cs' of
            (x :: _) =>
              case snd x of
                JArray cs'' =>
                  let cs''' = map (\c => case c of
                                           JObject c' =>
                                             case c' of
                                               ((_, (JDouble xval)) :: (_, (JDouble yval)) :: (_, (JDouble zval)) :: _) =>
                                                 
                                               _                                                                        =>
                                                 Nothing
                                           _             =>
                                             Nothing
                                  ) cs''
                    in pure $ MkCoordinates cs'''
                _           =>
                  die "Input not in proper format."
            _         =>
              die "Input not in proper format."
          --pure coord
        Right _             =>
          die "Input not in proper format."
    op :  Res
       -> Maybe Coordinate
       -> Res
    op (MkRes xsum ysum zsum count)
       (Just (MkCoordinate x y z)) =
         MkRes (xsum  + x)
               (ysum  + y)
               (zsum  + z)
               (count `plus` 1)
    op (MkRes xsum ysum zsum count)
       Nothing                     =
         MkRes xsum
               ysum
               zsum
               count

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
  {-
  let test = "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}"
    in case parseJSON Virtual test of
         Left _ => putStrLn "awfe"
         Right json => putStrLn $ show json
  -}
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
-}
