{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Lib
import Text.Read (readMaybe)
import Text.Regex.Posix
import Data.Array
import Data.Array.Unboxed
import Text.Show.Unicode
import System.Random.MWC


readInt :: String -> Maybe Int
readInt = readMaybe

data0 :: UArray (Int, Int) Int
data0 = Data.Array.Unboxed.array ((0,0),(9,9))[((0,0),1),((0,1),1),((0,2),1),((0,3),1),((0,4),1),((0,5),1),((0,6),1),((0,7),1),((0,8),1),((0,9),1),
                                               ((1,0),1),((1,1),2),((1,2),2),((1,3),2),((1,4),2),((1,5),2),((1,6),2),((1,7),2),((1,8),2),((1,9),1),
                                               ((2,0),1),((2,1),2),((2,2),2),((2,3),2),((2,4),2),((2,5),2),((2,6),2),((2,7),2),((2,8),2),((2,9),1),
                                               ((3,0),1),((3,1),2),((3,2),2),((3,3),2),((3,4),2),((3,5),2),((3,6),2),((3,7),2),((3,8),2),((3,9),1),
                                               ((4,0),1),((4,1),2),((4,2),2),((4,3),2),((4,4),3),((4,5),4),((4,6),2),((4,7),2),((4,8),2),((4,9),1),
                                               ((5,0),1),((5,1),2),((5,2),2),((5,3),2),((5,4),4),((5,5),3),((5,6),2),((5,7),2),((5,8),2),((5,9),1),
                                               ((6,0),1),((6,1),2),((6,2),2),((6,3),2),((6,4),2),((6,5),2),((6,6),2),((6,7),2),((6,8),2),((6,9),1),
                                               ((7,0),1),((7,1),2),((7,2),2),((7,3),2),((7,4),2),((7,5),2),((7,6),2),((7,7),2),((7,8),2),((7,9),1),
                                               ((8,0),1),((8,1),2),((8,2),2),((8,3),2),((8,4),2),((8,5),2),((8,6),2),((8,7),2),((8,8),2),((8,9),1),
                                               ((9,0),1),((9,1),1),((9,2),1),((9,3),1),((9,4),1),((9,5),1),((9,6),1),((9,7),1),((9,8),1),((9,9),1)]

-- (y,x),data
-- 0:設定なし
-- 1:枠外
-- 2:コマなし
-- 3:白
-- 4:黒
-- 5:置けるところ



muki = [(-1,-1) -- 0:左上
      , (-1, 0) -- 1:上
      , (-1, 1) -- 2:右上
      , ( 0,-1) -- 3:左
      , ( 0, 1) -- 4:右
      , ( 1,-1) -- 5:左下
      , ( 1, 0) -- 6:下
      , ( 1, 1) -- 7:右下
               ]
-- (y, x)

mukiLen = length muki


-- 隣の隣を調べる
func12 :: UArray (Int,Int) Int -> (Int, Int) -> Int -> [(Int, Int)] -> Maybe Bool -> Int -> Int -> IO (Maybe Bool)
func12 bd (y,x) i s status jibun aite = do

    status4 <- if y<1 || y>8 || x<1 || x>8
                   then do
                            return Nothing
                   else do
                            let atai3 = bd Data.Array.Unboxed.! (y,x)
                            status3 <- case atai3 of
                                           2 -> do    -- コマなし
                                                    return Nothing

                                           jibun -> do    -- 自分と同じ色のコマあり。挟めている
                                                    return (Just True)

                                           aite -> do    -- 相手と同じ色のコマあり。相手のコマが続くので更に調べる　再帰

                                                    let (sy,sx) = (s !! i)

                                                    let y2 = y + sy

                                                    let x2 = x + sx

                                                    let pos4 = (y2,x2)

                                                    status2 <- func12 bd pos4 i s status jibun aite
                                                    return status2

                                           _ -> do
                                                    return Nothing
                            return status3
    return status4


-- x方向を調べる。そこにコマがあるかないか？
-- func11 :: UArray (Int,Int) Int -> Int -> Int -> UArray (Int,Int) Int -> Int -> Int -> [(Int,Int)]
func11 bd py px okeruList jibun aite = do
    okeruList4 <- if px == 0
                      then do
                               return okeruList
                      else do
                               let atai = bd Data.Array.Unboxed.! (py,px)

                               okeruList3 <- case atai of
                                                 2 -> do
                                                          kekka <- func9 bd py px (mukiLen-1) (Nothing) jibun aite    -- 周りを知らべる
                                                          let okeruList2 = case kekka of
                                                                            Just True -> okeruList ++ [(py, px)]
                                                                            Just False -> okeruList
                                                                            Nothing -> okeruList
                                                                            _ -> okeruList
                                                          return okeruList2

                                                 jibun -> do    -- 自分と同じ色のコマ 
                                                          return okeruList

                                                 aite -> do    -- 相手と同じ色のコマ
                                                          return okeruList
                                                 _ -> do
                                                          return okeruList

                               func11 bd py (px-1) okeruList3 jibun aite
    return okeruList4


-- y方向を調べる
-- func10 :: UArray (Int,Int) Int -> Int -> Int UArray (Int,Int) Int -> Int -> Int -> IO([(Int,Int)]) 
func10 bd py px okeruList jibun aite = do
    okeruList4 <- if py == 0 
                      then do
                               return okeruList
                      else do
                               okeruList2 <- func11 bd py px okeruList jibun aite
                               okeruList3 <- func10 bd (py-1) px okeruList2 jibun aite
                               return okeruList3
    return okeruList4

-- 周りを調べる
func9 :: UArray (Int,Int) Int -> Int -> Int -> Int -> Maybe Bool -> Int -> Int -> IO(Maybe Bool)
func9 bd py px mukii status jibun aite = do
    status5 <- if mukii == -1 || status == (Just True)
                   then do
                            return status
                   else do
                            let (piy,pix) = muki !! mukii
                            let pmy = py + piy
                            let pmx = px + pix
                            let ataim = bd Data.Array.Unboxed.! (pmy,pmx)
                            status3 <- case ataim of
                                           aite -> do -- 相手と同じ色のコマあり
-- 隣の隣を調べる
                                                    status2 <- func12 bd (pmy,pmx) mukii muki status jibun aite    -- 継続操作
                                                    return status2

                                           _ -> do
                                                    return status


                            status4 <- func9 bd py px (mukii-1) status3 jibun aite    -- 次の方向を調べる
                            return status4
    return status5


-- 表示処理
func8 :: UArray (Int,Int) Int -> IO()
func8 bd = do
    putStrLn ("　　ａ　ｂ　ｃ　ｄ　ｅ　ｆ　ｇ　ｈ　Ｘ")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("１＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (1,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("２＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (2,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("３＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (3,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("４＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (4,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("５＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (5,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("６＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (6,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("７＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (7,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("８＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,1))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,2))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,3))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,4))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,5))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,6))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,7))) ++ "＊" ++
                 (func4 (bd Data.Array.Unboxed.! (8,8))) ++ "＊")
    putStrLn ("　＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊")
    putStrLn ("Ｙ")
    putStrLn ("[　] 0:nothing")
    putStrLn ("[〇] 1:Black")
    putStrLn ("[●] 2:White")


-- 盤面変更処理
-- func7 UArray (Int, Int) Int -> [(Int, Int)] -> Int -> Int -> IO(UArray (Int,Int) Int)
func7 bd hkrListKekka hkrListKekkaLen atai = do
    bd3 <- if hkrListKekkaLen == 0
               then do
                        return bd
               else do
                        let (posY, posX) = (hkrListKekka !! 0)
                        let bd2 = bd Data.Array.Unboxed.// [((hkrListKekka !! 0), atai)]
                        func7 bd2 (tail hkrListKekka) (hkrListKekkaLen-1) atai
    return bd3


-- 隣の隣以降を調べる。挟めているかどうか判定する
func6 :: (UArray (Int,Int) Int) -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)] -> Int -> Int -> IO([(Int, Int)])
func6 bd (y,x) hkrList2 i muki jibun aite = do
    hkrList5 <- if y<1 || y>8 || x<1 || x>8
                    then do
                             return []
                    else do
                             let atai3 = bd Data.Array.Unboxed.! (y,x)
                             hkrList4 <- case atai3 of
                                             2     -> do
                                                          return []
-- 自分と同じ色のコマあり。挟めている
                                             jibun -> do
                                                          return hkrList2
-- 相手のコマが続くので更に調べる　再帰
                                             aite  -> do
                                                         let hkrList3 = hkrList2 ++ [(y,x)]
                                                         let (sy,sx) = (muki !! i)
                                                         let y2 = y + sy
                                                         let x2 = x + sx
                                                         let pos4 = (y2,x2)
                                                         hkrList6 <- func6 bd pos4 hkrList3 i muki jibun aite
--                                                          hkrList6 <- func6 bd pos4 [(1,1),(2,2)] i [(1,1),(2,2)] jibun aite
                                                         return hkrList6

                                             _     -> return []
                             return hkrList4
--                              return ([(1,1),(2,2)])
    return hkrList5


-- 隣を調べる、隣が相手の駒なら、その隣をfunc6で調べる　すべての方向を調べる
func5 :: (UArray (Int, Int) Int) -> Int -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> IO  [(Int,Int)]
func5 bd i (pyn,pxn) muki mukiLen hkrWorkList hkrFinalList jibun aite = do

    hkrList7 <- if i == mukiLen
        then do
                 return hkrFinalList
        else do
               let (y,x) = muki !! i

               let posY2 = pyn + y

               let posX2 = pxn + x

               hkrList5 <- if posY2<1 || posY2>8 || posX2<1 || posX2>8
                               then do
                                        return []
                               else do
                                        let atai = bd Data.Array.Unboxed.! (posY2, posX2)

                                        hkrList4 <- case atai of

-- コマなし
                                                        2 -> do
                                                                 return []
-- 自分と同じ石
                                                        jibun -> do
                                                                 return []

-- 相手と同じ石
                                                        aite -> do
                                                                 let hkrList2 = [(pyn, pxn), (posY2, posX2)]

                                                                 let pos3 = (posY2 + y, posX2 + x)

                                                                 hkrList3 <- func6 bd pos3 hkrList2 i muki jibun aite

                                                                 return hkrList3

                                                        _ -> do
                                                                 return []
                                        return hkrList4

               let hkrList6 = hkrFinalList ++ hkrList5

               func5 bd (i+1) (pyn, pxn) muki mukiLen hkrWorkList hkrList6 jibun aite

    return hkrList7

-- コード　→　表示文字　への変換
-- func4 :: UArray Int Int -> String
func4 ua1 = do
    let a = ["？","※","　","●","〇","！"]
    a !! ua1


-- そこにコマがないかどうか、なければfunc5に処理を移す
-- func3 :: UArray(Int, Int) Int -> Int -> Int -> IO(UArray (Int,Int) Int)
func3 bd jibun aite = do
    print ("aite =" ++ show aite)
-- (y, x)
--     let s = [(-1,-1) -- 0:左上
--            , (-1, 0) -- 1:上
--            , (-1, 1) -- 2:右上
--            , ( 0,-1) -- 3:左
--            , ( 0, 1) -- 4:右
--            , ( 1,-1) -- 5:左下
--            , ( 1, 0) -- 6:下
--            , ( 1, 1) -- 7:右下
--             ]

--     let sLen = length s

    print ("aite = " ++ show aite)
    command <- func2
    print ("aite = " ++ show aite)
    putStrLn("command = " ++ command)

    let py = command !! 1 -- ２文字目　Ｙ座標

    let pyn = read [py] :: Int

    let pxa = command !! 0 -- １文字目　Ｘ座標

    let px = case pxa of
                        'a' -> '1'
                        'b' -> '2'
                        'c' -> '3'
                        'd' -> '4'
                        'e' -> '5'
                        'f' -> '6'
                        'g' -> '7'
                        'h' -> '8'
                        _   -> '0'

    let pxn = read [px] :: Int

    let a = bd Data.Array.Unboxed.! (pyn, pxn) -- 選択場所の値を取得
    print ("a = " ++ show a)
    print ("aite = " ++ show aite)

-- 相手の同じ石あり
    bd3 <- case a of
--                aite -> do
               3 -> do
                           print ("aite = " ++ show aite)
                           putStrLn( "相手の処理")
                           bd4 <- func3 bd jibun aite    -- このファンクションの最初に戻る
                           return bd4

-- 自分と同じ石あり
--                jibun -> do
               4 -> do
                            putStrLn("自分の処理")
                            bd4 <- func3 bd jibun aite    -- このファンクションの最初に戻る
                            return bd4
-- コマなし
               2 -> do
                        print "2の処理"
                        let hkrWorkList = []
                        let hkrFinalList = []
--                      hkrListKekka <- func5 bd 0 (pyn, pxn) s sLen hkrWorkList hkrFinalList jibun aite
                        hkrListKekka <- func5 bd 0 (pyn, pxn) muki mukiLen hkrWorkList hkrFinalList jibun aite
-- ひっくり返し処理
                        let hkrListKekkaLen = length hkrListKekka 
                        bd2 <- func7 bd hkrListKekka hkrListKekkaLen jibun 
                        func8 bd2    -- 表示処理
                        return bd2
               _ -> do
                        print "_を処理"
                        return bd
    return bd3


-- 置き場所入力
func2 = do
    putStrLn("")
    putStrLn("置く場所を入力してください。　例　a1　※半角英数　列行の順 ")
    a <- getLine

    case a of
        x | (length x) /= 2 -> do
                putStrLn "２文字で入力してください"
                func2 -- やり直し

          | ([x !! 0] =~ "[abcdefgh]" :: Bool) == False  || ([x !! 1] =~ "[12345678]" :: Bool) == False -> do
                putStrLn "入力された文字が間違っています"
                func2 -- やり直し

          | otherwise -> do
                putStrLn ("You selected " ++ a)
                return a


-- start
func1 :: UArray (Int, Int) Int -> IO(UArray (Int, Int) Int)
func1 bd = do

    func8 bd    -- 表示処理

-- 黒番
    let jibun = 4    -- 黒
    let aite = 3    -- 白
    putStrLn("黒番です")
    bd2 <- func3 bd jibun aite
    func8 bd2
    let bd = bd2

-- 白版
    let jibun = 3
    let aite = 4  
    putStrLn("白番です")
-- 置けるところを探す
    let py = 8
    let px = 8

    okeruList <- func10 bd py px [] jibun aite
-- 置けるところに「！」を表示する
    let okeruListLen = length okeruList
    bd2 <- func7 bd okeruList okeruListLen 5 
    func8 bd2    -- 表示処理

-- 置き場所を選ぶ
    gen <- createSystemRandom
    okibasyoNum <- uniformR(1,okeruListLen) gen :: IO Int

    putStrLn("置き場所 = " ++ show (okeruList !! (okibasyoNum-1)))    

    let okibasyo = okeruList !! (okibasyoNum-1)

    let hkrWorkList = []
    let hkrFinalList = []
    hkrListKekka <- func5 bd2 0 okibasyo muki mukiLen hkrWorkList hkrFinalList jibun aite

-- ひっくり返し処理
    let hkrListKekkaLen = length hkrListKekka
    bd2 <- func7 bd hkrListKekka hkrListKekkaLen jibun    -- 白で更新

    func8 bd2    -- 表示処理



--     let bd3 = bd2 Data.Array.Unboxed.// [(okibasyo,3)]
--    putStrLn("bd3 = " ++ show bd3)

--     func8 bd3
-- ひっくり返し処理







-- 黒番に戻る
    func1 bd2


main :: IO ()
main = do
    someFunc

    let bd = data0

    func1 bd

    return ()

