import Stewbot
import qualified Network.Wreq.Session as S
import System.Process

main = do
    bot <- makeBot
    runSearch bot "list" >>= writeFile "out.html"
    callCommand "firefox out.html"

oldmain = do
    bot <- makeBot
    res <- addItems bot
        [Item "item_79003957"   1 Best
        ,Item "item_679798180"  1 Best
        ,Item "item_78959876"   1 Best
        ,Item "item_79088941"   1 Best
        ,Item "item_78969823"   2 Best
        ,Item "item_78971804"   3 Best
        ,Item "item_78971249"   1 Best
        ,Item "item_78971557"   3 Best
        ,Item "item_83013875"   6 No
        ,Item "item_79019981"   8 No
        ,Item "item_1368015104" 8 No
        ,Item "item_169853881"  2 No
        ,Item "item_79020062"   1 No
        ,Item "item_130991374"  1 No
        ,Item "item_747904715"  1 No
        ,Item "item_78966249"   3 No
        ]
    putStrLn . show $ res
