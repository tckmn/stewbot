import Stewbot
import qualified Network.Wreq.Session as S

main = do
    sess <- makeSession
    putStrLn . show $ sess
