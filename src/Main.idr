module Main

import Derive.Prelude
import JSON.Parser
--import Language.Reflection.Util
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

record Coordinates where
  constructor MkCoordinates
  coords : List (Maybe Coordinate)

%runElab derive "Coordinates" [Show,Eq]

-- %runElab derive "Coordinates" [Show,Eq,ToJSON,FromJSON]

--coords : Value -> Parser (List Coordinate)
--coords = withObject "coords" (field "coordinates")

data Res : Type where
  MkRes :  Double
        -> Double
        -> Double
        -> Nat
        -> Res

calc :  String
     -> IO Coordinate
calc f = do
  (MkCoordinates coords)           <- coordinates f
  let (MkRes xsum ysum zsum count) = foldl op (MkRes 0 0 0 0) coords
      c                            = cast count
  pure $
    MkCoordinate (floor $ xsum / c)
                 (floor $ ysum / c)
                 (floor $ zsum / c)
  where
    coordinates :  String
                -> IO Coordinates
    coordinates cs =
      --case decode {a = Coordinates} cs of
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
                                                 Just $ MkCoordinate xval yval zval
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
