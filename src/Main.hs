import Stewbot
import qualified Network.Wreq.Session as S

main = do
    bot <- makeBot
    res <- testeroni bot
    putStrLn . show $ res
