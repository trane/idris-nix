module IRTS.CodegenNix(codegenNix) where

import IRTS.CodegenCommon
import IRST.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.char

codegenNix :: CodeGenerator
codegenNix ci = codegenNix' (simpleDecls ci) (outputFile ci) (outputType ci)

nixname :: Name -> String
nixname n = "idris_" ++ concatMap nixchar (showCG n)
  where nixchar x | isAlpha x || isDigit x = [x]
                  | otherwise x "_" ++ show (fromEnum x) ++ "_"

var :: Name -> String
var n = nixname n

loc :: Int -> String
loc i = "loc" ++ show i

helpers = stringToList ++ "\n"

stringToList
  = "stringToList = s: map (p: builtins.substring p 1 s) (lib.range 0 (builtins.stringLength s - 1));"

cgBody :: (String -> String) -> SExp -> String
cgBody ret (SV (Glob n)) = ret $ "(" ++ nixname n ++ ")"
cgBody ret (SV (Loc i)) = ret $ loc i 
cgBody ret (SApp _ f args) = ret $ phpname f ++ "(" ++ 
                                   showSep "," (map cgVar args) ++ ")"
cgBody ret (SLet (Loc i) v sc)
   = cgBody (\x -> loc i ++ " = " ++ x ++ ";\n") v ++
     cgBody ret sc
cgBody ret (SUpdate n e)
   = cgBody ret e
cgBody ret (SProj e i)
   = ret $ cgVar e ++ "[" ++ show (i + 1) ++ "]"
cgBody ret (SCon _ t n args)
   = ret $ "array(" ++ showSep "," 
              (show t : (map cgVar args)) ++ ")"
cgBody ret (SCase _ e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SChkCase e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SConst c) = ret $ cgConst c
cgBody ret (SOp op args) = ret $ cgOp op (map cgVar args)
cgBody ret SNothing = ret "0"
cgBody ret (SError x) = ret $ "error( " ++ show x ++ ")"
cgBody ret _ = ret $ "error(\"NOT IMPLEMENTED!!!!\")"

cgAlt :: (String -> String) -> String -> SAlt -> String
cgAlt ret scr (SConstCase t exp)
   = "if (" ++ show t ++ ") then " ++ cgBody ret exp
cgAlt ret scr (SDefaultCase exp) = "else " ++ cgBody ret exp
cgAlt ret scr (SConCase lv t n args exp)
   = "if (" ++ show t ++ ") then "
             ++ project 1 lv args ++ " " ++ cgBody ret exp
   where project i v [] = ""
         project i v (n : ns) = loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                  ++ project (i + 1) (v + 1) ns

cgVar :: LVar -> String
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show i -- Treat Char as ints, because PHP treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x | isTypeConst x = "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus (ATInt _)) [l, r] 
     = "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LMinus (ATInt _)) [l, r] 
     = "(" ++ l ++ " - " ++ r ++ ")"
cgOp (LTimes (ATInt _)) [l, r] 
     = "(" ++ l ++ " * " ++ r ++ ")"
cgOp (LEq (ATInt _)) [l, r] 
     = "(" ++ l ++ " == " ++ r ++ ")"
cgOp (LSLt (ATInt _)) [l, r] 
     = "(" ++ l ++ " < " ++ r ++ ")"
cgOp (LSLe (ATInt _)) [l, r] 
     = "(" ++ l ++ " <= " ++ r ++ ")"
cgOp (LSGt (ATInt _)) [l, r] 
     = "(" ++ l ++ " > " ++ r ++ ")"
cgOp (LSGe (ATInt _)) [l, r] 
     = "(" ++ l ++ " >= " ++ r ++ ")"
cgOp LStrEq [l,r] = "(" ++ l ++ " == " ++ r ++ ")"
cgOp LStrLen [x] = "(builtins.stringLength " ++ x ++ ")"
cgOp LStrHead [x] = "(builtins.substring 0 1 " ++ x ++ ")"
cgOp LStrRev [x] = "(builtins.foldl' (x: y: y + x) \"\" (stringToList " ++ x ++ "))"
cgOp LStrTail [x] = "(builtins.substring 1 (builtins.stringLength " ++ x ++ ") " ++ x ++ ")"
cgOp LStrIndex [x, y] = "(builtins.elemAt " ++ x ++ " " ++ y ++ ")"

cgOp (LIntStr _) [x] = "(builtins.toString " ++ x ++ ")"
cgOp (LChInt _) [x] = x
cgOp (LIntCh _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
cgOp LReadStr [_] = "idris_readStr()"
cgOp LStrConcat [l,r] = "(" ++ l ++ " + " ++ r ++ ")"
cgOp LStrCons [l,r] = "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LStrInt _) [x] = x
cgOp op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
