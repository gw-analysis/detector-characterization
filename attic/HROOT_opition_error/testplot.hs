import Control.Monad
import HROOT

main :: IO ()
main = do
     tapp <- newTApplication "test" [0] ["test"]
     tcanvas <- newTCanvas "Test" "Test" 640 480
     let x = [0, 0.1..6.28]
     let y = map sin x

     g1 <- newTGraph (length x) x y
     setLineColor g1 2
--     setMarkerColor g1 2
     setLineWidth g1 2
     draw g1 "AL*"
     run tapp 1

     delete g1
     delete tapp

