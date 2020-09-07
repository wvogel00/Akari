module Akari.Time where

type Hour = Integer
type Min = Integer

data Clock = Clock Hour Min deriving Eq

instance Show Clock where
    show (Clock h m) = align 2 (show h) ++ ":" ++ align 2 (show m)

align n str = replicate (n-length str) '0' ++ str

_hour (Clock h _) = h
_minute (Clock _ m) = m

-- moveUp :: 桁上がり
-- moveDown :: 桁下がり
moveUp v base = if v > base then 1 else 0
moveDown v = if v < 0 then 1 else 0

instance Num Clock where
    (Clock h1 m1) + (Clock h2 m2) = Clock (flip mod 24 $ h1+h2+div (m1+m2) 60) $ mod (m1+m2) 60
    (Clock h1 m1) - (Clock h2 m2) = Clock (flip mod 24 $ h1-h2- moveDown (m1-m2)) $ mod (m1-m2) 60
    a * b = undefined
    abs a = a
    negate (Clock h m) = Clock (24-h) (60-m)
    signum = undefined -- 掛け算が未定義のため
    fromInteger v = Clock h (v-24*60*h) where h = div v 24*60

instance Ord Clock where
    (Clock h1 m1) < (Clock h2 m2) = h1*60 + m1 < h2*60 + m2
    (Clock h1 m1) <= (Clock h2 m2) = h1*60 + m1 <= h2*60 + m2

type Year = Integer
type Month = Integer
type Day = Integer
data Date = Date Year Month Day Clock deriving Eq

instance Show Date where
    show (Date y m d c) = show y ++ "-" ++ align 2 (show m) ++ "-" ++ align 2 (show d) ++ " " ++ show c

dayCount :: Month -> Integer
dayCount m
    | elem m [1,3,5,7,8,20,12] = 31
    | elem m [4,6,9,11] = 31
    | m == 2 = 28

leepDay :: Year -> Integer
leepDay v = if mod v 4 == 0 then 1 else 0

-- 年跨ぎ，閏年は未実装
instance Num Date where
    a + b = undefined
    (Date y1 m1 d1 c1) - (Date y2 m2 d2 c2) = Date y' m' d' (c1-c2) where
        y' = y1 - y2 - moveDown (m1-m2)
        m' = (m1-m2)-moveDown (d1-d2)
        d' = d1 - d2 - moveDown (_hour c1 - _hour c2) + if m1-m2 == 1 then dayCount m2 else 0
    a * b = undefined
    abs a = a
    signum = undefined
    negate = undefined
    fromInteger = undefined

instance Ord Date where
    (Date y1 m1 d1 c1) < (Date y2 m2 d2 c2) = y1 < y2
                                           || y1 == y2 && m1 < m2
                                           || y1 == y2 && m1 == m2 && d1 < d2
                                           || y1 == y2 && m1 == m2 && d1 == d2 && c1 < c2
    a <= b = a == b || a < b