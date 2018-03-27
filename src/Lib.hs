module Lib
    ( someFunc,
        Letter(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z),
        rotor_I,
        rotor_II,
        rotor_III,
        ukw_B,
        Rotor(..),
        adjustLetterForward,
        adjustLetterReverse,
        transposeForward,
        transposeReverse,
        EnigmaMachine(..),
        encipher,
        step,
        rotorAtTurnoverPosition,
        stepLeftRotor,
        stepMiddleRotor,
        stepRightRotor,
        stepMachine
    ) where

import qualified Math.Algebra.Group.PermutationGroup as MAGP
import qualified Math.Core.Utils as MCU
import qualified Control.Monad.State

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show, Eq, Ord)

rotor_I = MAGP.fromList [U,W,Y,G,A,D,F,P,V,Z,B,E,C,K,M,T,H,X,S,L,R,I,N,Q,O,J]
rotor_II = MAGP.fromList [A,J,P,C,Z,W,R,L,F,B,D,K,O,T,Y,U,Q,G,E,N,H,X,M,I,V,S]
rotor_III = MAGP.fromList [T,A,G,B,P,C,S,D,Q,E,U,F,V,N,Z,H,Y,I,X,J,W,L,R,K,O,M]
ukw_B = MAGP.fromList [Y,R,U,H,Q,S,L,D,P,X,N,G,O,K,M,I,E,B,F,Z,C,W,V,J,A,T]

data Rotor = Rotor { wiring :: (MAGP.Permutation Letter)
                    , ring_setting :: Letter
                    , starting_position :: Letter
                    }

instance Show Rotor where
    show (Rotor w r p) = show r ++ " " ++ show p

data EnigmaMachine = EnigmaMachine { reflector :: Rotor,
                            right_rotor :: Rotor,
                            middle_rotor :: Rotor,
                            left_rotor :: Rotor
                        }

instance Show EnigmaMachine where
    show (EnigmaMachine r rr rm rl) = show rl ++ "    " ++ show rm ++ "    "  ++ show rr

step :: Rotor -> Rotor
step (Rotor w r p) = Rotor w r (succ p)

rotorAtTurnoverPosition :: Rotor -> Bool
rotorAtTurnoverPosition (Rotor w r p) =
    case w == rotor_I of
        True -> p == R
        False -> case w == rotor_II of
            True -> p == F
            False -> case w == rotor_III of
                True -> p == W
                False -> False

rotorAlmostAtTurnoverPosition :: Rotor -> Bool
rotorAlmostAtTurnoverPosition (Rotor w r p) =
    case w == rotor_I of
        True -> p == Q
        False -> case w == rotor_II of
            True -> p == E
            False -> case w == rotor_III of
                True -> p == V
                False -> False

stepRightRotor :: Rotor -> Rotor
stepRightRotor = step

stepMiddleRotor :: Rotor -> Rotor -> Rotor -> Rotor
stepMiddleRotor rl rm rr =
    if rotorAlmostAtTurnoverPosition rr then step rm else
        if rotorAlmostAtTurnoverPosition rm && rotorAlmostAtTurnoverPosition rl then step rm else rm

stepLeftRotor :: Rotor -> Rotor -> Rotor
stepLeftRotor rl rm = if rotorAlmostAtTurnoverPosition rm then step rl else rl

stepMachine :: EnigmaMachine -> EnigmaMachine
stepMachine (EnigmaMachine r rr rm rl) = (EnigmaMachine r (stepRightRotor rr) (stepMiddleRotor rl rm rr) (stepLeftRotor rl rm))

adjustLetterForward :: Letter -> Int -> Letter
adjustLetterForward l i = 
    let adjust = if i >= 0 then foldr (.) id (replicate i succ) else foldr (.)  id (replicate (26 + i) succ)
    in adjust l

adjustLetterReverse :: Letter -> Int -> Letter
adjustLetterReverse l i =
    let adjust = if i >= 0 then foldr (.) id (replicate i pred) else foldr (.) id (replicate (26 + i) pred)
    in adjust l

applyRingSettingPre :: Letter -> Letter -> Letter
applyRingSettingPre r l = adjustLetterReverse l (fromEnum r)

applyRotorPositionPre :: Letter -> Letter -> Letter
applyRotorPositionPre p l = adjustLetterForward l (fromEnum p)


applyRotorTransform :: MAGP.Permutation Letter -> Letter -> Letter
applyRotorTransform w l = l MAGP..^ w

applyRotorTransformInverse :: MAGP.Permutation Letter -> Letter -> Letter
applyRotorTransformInverse w l = l MAGP..^ (w MCU.^- 1)

applyRotorPositionPost :: Letter -> Letter -> Letter
applyRotorPositionPost p l = adjustLetterReverse l (fromEnum p)

applyRingSettingPost :: Letter -> Letter -> Letter
applyRingSettingPost r l = adjustLetterForward l (fromEnum r)

-- Total calculation of transforming a letter with a rotor, including position and ring setting, from right to left
transposeForward :: Rotor -> Letter -> Letter
transposeForward (Rotor {wiring = w, ring_setting = r, starting_position = p}) l =
    ((applyRingSettingPost r) . (applyRotorPositionPost p) . (applyRotorTransform w) . (applyRotorPositionPre p) . (applyRingSettingPre r)) l

-- Total calculation of transforming a letter with a rotor, including position and ring setting, from left to right
transposeReverse :: Rotor -> Letter -> Letter
transposeReverse (Rotor {wiring = w, ring_setting = r, starting_position = p}) l =
    ((applyRingSettingPost r) . (applyRotorPositionPost p) . (applyRotorTransformInverse w) . (applyRotorPositionPre p) . (applyRingSettingPre r)) l

encipher ::  EnigmaMachine -> Letter -> Letter
encipher (EnigmaMachine {reflector = r, right_rotor = r1, middle_rotor = r2, left_rotor = r3}) l =
    ((transposeReverse r1) . (transposeReverse r2) . (transposeReverse r3) . (transposeForward r) . (transposeForward r3) . (transposeForward r2) . (transposeForward r1)) l

instance Enum Letter where
    toEnum 0 = A
    toEnum 1 = B
    toEnum 2 = C
    toEnum 3 = D
    toEnum 4 = E
    toEnum 5 = F
    toEnum 6 = G
    toEnum 7 = H
    toEnum 8 = I
    toEnum 9 = J
    toEnum 10 = K
    toEnum 11 = L
    toEnum 12 = M
    toEnum 13 = N
    toEnum 14 = O
    toEnum 15 = P
    toEnum 16 = Q
    toEnum 17 = R
    toEnum 18 = S
    toEnum 19 = T
    toEnum 20 = U
    toEnum 21 = V
    toEnum 22 = W
    toEnum 23 = X
    toEnum 24 = Y
    toEnum 25 = Z
    fromEnum A = 0
    fromEnum B = 1
    fromEnum C = 2
    fromEnum D = 3
    fromEnum E = 4
    fromEnum F = 5
    fromEnum G = 6
    fromEnum H = 7
    fromEnum I = 8
    fromEnum J = 9
    fromEnum K = 10
    fromEnum L = 11
    fromEnum M = 12
    fromEnum N = 13
    fromEnum O = 14
    fromEnum P = 15
    fromEnum Q = 16
    fromEnum R = 17
    fromEnum S = 18
    fromEnum T = 19
    fromEnum U = 20
    fromEnum V = 21
    fromEnum W = 22
    fromEnum X = 23
    fromEnum Y = 24
    fromEnum Z = 25
    succ x = case x of
        Z -> A
        _ -> toEnum (((fromEnum x) + 1) :: Int) :: Letter
    pred x = case x of
        A -> Z
        _ -> toEnum (((fromEnum x) - 1) :: Int) :: Letter

someFunc :: IO ()
someFunc = putStrLn "someFunc"