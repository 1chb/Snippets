-- a^b * a^c === a^(b+c)

from :: (b -> a, c -> a) -> (Either b c -> a)
from = \(f, g) x -> case x of Left b -> f b; Right c -> g c

to :: (Either b c -> a) -> (b -> a, c -> a)
to = \h -> (h . Left, h . Right)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) = \f g x -> f (g x)

id :: a -> a
id = \x -> x

{- ORMOLU_DISABLE -}
p00 = from . to === id
p01 = (.) from to === id -- postfix
p02 = \f g x -> f (g x) from to === id -- definition of `(.)`
p03 = \g x -> from (g x) to === id -- beta reduction of `f`
p04 = \x -> from (to x) === id -- beta reduction of `g`
p05 = \x -> from ((\h -> (h . Left, h . Right)) x) === id -- definition of `to`
p06 = \x -> from (((x . Left, x . Right))) === id -- beta reduction of 'x'
p07 = \x -> from (x . Left, x . Right) === id -- 2*redundant parenthesis
p08 = \h -> from (h . Left, h . Right) === id -- alpha reduction `x'->`h`
p09 = \h -> (\(f,g) x -> case x of Left b -> f b; Right c -> g c) (h . Left, h . Right) === id -- definition of `from`
p10 = \h -> (\x -> case x of Left b -> (h . Left) b; Right c -> (h . Right) c) === id -- beta reduction
p11 = \h x -> case x of Left b -> (h . Left) b; Right c -> (h . Right) c === id -- redundant parenthesis + lambda syntax
p12 = \h x -> case x of Left b -> ((.) h Left) b; Right c -> ((.) h Right) c === id -- 2*postfix `(.)`
p13 = \h y -> case y of Left b -> (.) h Left b; Right c -> (.) h Right c === id -- alpha reduction `x`->`y` + 2*redundant parenthesis
p14 = \h y -> case y of Left b -> (\f g x -> f (g x)) h Left b; Right c -> (\f g x -> f (g x)) h Right c === id -- 2*definition of `.`
p15 = \h y -> case y of Left b -> (\g x -> h (g x)) Left b; Right c -> (\g x -> h (g x)) Right c === id -- 2*beta reduction of `f`
p16 = \h y -> case y of Left b -> (\x -> h (Left x)) b; Right c -> (\x -> h (Right x)) c === id -- 2*beta reduction of 'g'
p17 = \h y -> case y of Left b -> (h (Left b)); Right c -> (h (Right c)) === id -- 2*beta reduction of `x`
p18 = \h y -> case y of Left b -> h (Left b); Right c -> h (Right c) === id -- 2*redundant parenthesis
p19 = \h y -> h (case y of Left b -> (Left b); Right c -> (Right c)) === id -- extract common sub expression `h`
p20 = \h y -> h (case y of Left b -> Left b; Right c -> Right c) === id -- 2*redundant parenthesis
p21 = \h y -> h (case y of Left _ -> y; Right _ -> y) === id -- 2*reuse of 'y'
p22 = \h y -> h (y) === id -- reduce `case`
p23 = \h y -> h y === id -- redundant parenthesis
p24 = \h -> h === id -- eta reduction
p25 = \x -> x === id -- alpha reduction
p26 = id === id -- reversed definition of `id` => QED

q00 = to . from === id
q01 = (.) to from === id -- postfix `(.)`
q02 = (\f g x -> f (g x)) to from === id -- definition of `(.)`
q03 = (\g x -> to (g x)) from === id -- beta reduction of `f`
q04 = (\x -> to (from x)) === id -- beta reduction of `g`
q05 = \x -> to (from x) === id -- redundant parenthesis
q06 = \x -> (\h -> (h . Left, h . Right)) (from x) === id -- definition of `to`
q07 = \x -> (((from x) . Left, (from x) . Right)) === id -- beta reduction of `h`
q08 = \x -> ((from x) . Left, (from x) . Right) === id -- redundant parenthesis
q09 = \x -> ((.) (from x) Left, (.) (from x) Right) === id -- 2*postfix `(.)`
q10 = \y -> ((.) (from y) Left, (.) (from y) Right) === id -- alpha reduction `x`->`y`
q11 = \y -> ((\f g x -> f (g x)) (from y) Left, (\f g x -> f (g x)) (from y) Right) === id -- 2*definition of `(.)`
q12 = \y -> ((\g x -> (from y) (g x)) Left, (\g x -> (from y) (g x)) Right) === id -- 2*beta reduction of `f`
q13 = \y -> ((\x -> (from y) (Left x)), (\x -> (from y) (Right x))) === id -- 2*beta reduction of `g`
q14 = \y -> (\x -> from y (Left x), \x -> from y (Right x)) === id -- 4*redundant parenthesis
q15 = \y -> (\z -> from y (Left z), \z -> from y (Right z)) === id -- 2*alpha reduction 'x'->'z'
q16 = \y -> (\z -> (\(f,g) x -> case x of Left b -> f b; Right c -> g c) y (Left z), \z -> (\(f,g) x -> case x of Left b -> f b; Right c -> g c) y (Right z)) === id -- 2*definition of `from`
q17 = \(h,i) -> (\z -> (\(f,g) x -> case x of Left b -> f b; Right c -> g c) (h,i) (Left z), \z -> (\(f,g) x -> case x of Left b -> f b; Right c -> g c) (h,i) (Right z)) === id -- alpha reduction `y`->`(h,i)`
q18 = \(h,i) -> (\z -> (\x -> case x of Left b -> h b; Right c -> i c) (Left z), \z -> (\x -> case x of Left b -> h b; Right c -> i c) (Right z)) === id -- beta reduction `f`->`h` and `g`->`i`
q19 = \(h,i) -> (\z -> (case (Left z) of Left b -> h b; Right c -> i c), \z -> (case (Right z) of Left b -> h b; Right c -> i c)) === id -- 2*beta reduction of `x`
q19 = \(h,i) -> (\z -> case Left z of Left b -> h b; Right c -> i c, \z -> case Right z of Left b -> h b; Right c -> i c) === id -- 4*redundant parenthesis
q20 = \(h,i) -> (\b -> case Left b of Left b -> h b; Right c -> i c, \c -> case Right c of Left b -> h b; Right c -> i c) === id -- alpha reduction `z`->`b` and `z`->`c`
q21 = \(h,i) -> (\b -> h b, \c -> i c) === id -- 2*case application
q22 = \(h,i) -> (h,i) === id -- 2*beta reduction
q23 = \x -> x === id -- alpha reduction of `(h,i`)
q24 = id === id -- reversed definition of `id` => QED
{- ORMOLU_ENABLE -}
